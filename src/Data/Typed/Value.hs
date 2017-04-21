{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass     ,NoMonomorphismRestriction #-}
module Data.Typed.Value(
  Value(..)
  ) where

import           Data.Char                      (chr)
import           Data.Flat
import           Data.Int
import           Data.Maybe
import           Data.Model.Class
import           Data.Model.Pretty
import           Data.Typed.Class
import           Data.Typed.Model               (Bytes)
import           Data.Typed.Types
import           Data.Word
import           Data.ZigZag
import           Text.PrettyPrint.HughesPJClass
import           Data.Typed.Pretty
import           Data.BLOB
import qualified Data.Text    as T
import qualified Data.Text.Encoding    as T
import qualified Data.Sequence         as S
import qualified Data.ByteString       as B
import Debug.Trace

-- Generic value (used for dynamic decoding)
data Value = Value {valType::AbsType -- Type
                   ,valName::String   -- Constructor name (duplicate info if we have abstype)
                   ,valBits::[Bool] -- Bit encoding/constructor id
                   -- TODO: add field names (same info present in abstype)
                   ,valFields::[Value]  -- Values to which the constructor is applied, if any
                   } deriving  (Eq,Ord,Show,NFData, Generic, Flat)

instance Flat [Bool]
instance Flat [Value]

{-
Pretty print Value as it would be the corresponding Haskell value
-}
instance Pretty Value where
     pPrintPrec (PrettyLevel lev) prec = pp 0
       where -- TODO: fix precedence
         pp l v@(Value t n bs vs) =
           let (complex,doc) = case prettyPrinter t of
                 Nothing -> (not (null vs),text n : map (pp (l+1)) vs)
                 Just pr -> let (c,d) = pr v in (c,[d])

           in (if complex && l>0 then parens else id) (hsep $ (if null bs || lev == 0 then empty else (text (map (\b -> if b then '1' else '0') bs) <> char ':')) : doc) -- : if lev == 0 then empty else )

         ch (Value _ _ _ [w]) = chr $ wl_ w
         ch v = error (show v)

         wrd = int . wrd_

         wrd_ (Value _ ('V':n) _ _) = read n :: Int -- Word64?
         wrd_ v                              = error (unwords ["wrd_",show v])

         wrd2_ (Value _ _ _ [Value _ ('V':n) _ _]) = read n :: Int
         wrd2_ v = error (unwords ["wrd2_",show v])

         -- i :: forall a . Proxy a -> (AbsType, Value -> (Bool, Doc))
         -- i p = (absType p,\v -> (False,int . (zzDecode::(Bits a,Integral a) => Word64 -> a) . wl_ . p0 . p0 $ v))

         wl = text . show . (wl_::Value -> Word64)

         wl_ (Value _ _ _ [Value _ _ _ [Value _ _ _ [vl]]]) = fromIntegral . fst . foldl (\(t,e) n -> (t+n*2^e,e+7)) (0,0) . map wrd2_ . neList $ vl

         wl_ v = error (unwords ["wl_",show v])

         valList (Value _ "Cons" _ [h,t]) = h:valList t
         valList (Value _ "Nil"  _ [])    = []
         valList v                      = error (unwords ["valList",show v])

         neList (Value _ "Cons" _ [h,t]) = h:neList t
         neList (Value _ "Elem"  _ [e])  = [e]
         neList v                      = error (unwords ["neList",show v])

         arrList (Value _ "A0" _ []) = []
         arrList (Value _ ('A':n) _ vs) | length vs == read n + 1 = init vs ++ arrList (last vs)

         arr = arr_ (pp 0)
         ar = prettyList_ (pp 0)

         tup = prettyTuple_ (pp 0)

         p0 (Value _ _ _ [v]) = v

         tuple (Value _ _ _ vs) = (False,tup vs)

         prettyPrinter t = listToMaybe . map snd . filter fst . map (\(t2,p) -> (match t t2,p)) $ [
            (absType (Proxy::Proxy ()),\_ -> (False,text "()"))
           ,(absType (Proxy::Proxy Word8),\v -> (False,wrd v))
           ,(absType (Proxy::Proxy Word16),\v -> (False,wl v))
           ,(absType (Proxy::Proxy Word32),\v -> (False,wl v))
           ,(absType (Proxy::Proxy Word64),\v -> (False,wl v))
           ,(absType (Proxy::Proxy Word),\v -> (False,wl v))
           ,(absType (Proxy::Proxy Int8),\v -> (False,int . zzDecode . wrd_ . p0 . p0 $ v))
           ,(absType (Proxy::Proxy Int16),\v -> (False,int . fromIntegral . zzDecode16 . wl_ . p0 . p0 $ v))
           ,(absType (Proxy::Proxy Int32),\v -> (False,int . fromIntegral . zzDecode32 . wl_ . p0 . p0 $ v))
           ,(absType (Proxy::Proxy Int64),\v -> (False,int . fromIntegral . zzDecode64 . wl_ . p0 . p0 $ v))
           ,(absType (Proxy::Proxy Int),\v -> (False,int . fromIntegral . zzDecode64 . wl_ . p0 . p0 $ v))
           ,(absType (Proxy::Proxy Integer),\v -> (False,int . fromIntegral . zzDecodeInteger . wl_ . p0 $ v))
           --,(absType (Proxy::Proxy Natural),\v -> (False,int . fromIntegral . zzDecodeInteger . wl_ . p0 $ v))
           ,(absType (Proxy::Proxy Float),\v -> (False,float $ floatVal v)) -- (False,int . fromIntegral . zzDecodeInteger . wl_ . p0 $ v))
           --,(absType (Proxy::Proxy Char),\v -> (False,text ['\'',ch v,'\'']))
           ,(absType (Proxy::Proxy Char),\v -> (False,pPrint (ch v)))
           --,(absType (Proxy::Proxy [Char]),\v -> (False,pPrint . map ch . valList $ v))
           ,(absType (Proxy::Proxy [Char]),\v -> (False,doubleQuotes . text . map ch . valList $ v))
           --,(absType (Proxy::Proxy ([Char])),\v -> (False,char '"' <> (text . map ch . valList $ v) <> char '"'))
           ,(absType (Proxy::Proxy [Any]),\v -> (False,ar (valList v)))
           ,(absType (Proxy::Proxy (NonEmptyList Any)),\v -> (False,ar (neList v)))
           ,(absType (Proxy::Proxy (S.Seq Char)),\v -> (False,pPrint . map ch . arrList $ v))
           ,(absType (Proxy::Proxy (S.Seq Any)),\v -> (False,arr (arrList v)))
           --,(absType (Proxy::Proxy (P.Array Char)),\v -> (False,pPrint . map ch . arrList $ v))
           ----,(absType (Proxy::Proxy (P.Array Any)),\v -> (True,arr (arrList v)))
           --,(absType (Proxy::Proxy (P.Array Any)),\v -> (False,arr (arrList v)))
           ,(absType (Proxy::Proxy Bytes),\v -> (False,pPrint . bytes $ v))
           ,(absType (Proxy::Proxy (BLOB UTF8Encoding)),utf8Text)
           ,(absType (Proxy::Proxy (BLOB UTF16LEEncoding)),utf16Text)
           ,(absType (Proxy::Proxy (Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any,Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any,Any,Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any,Any,Any,Any,Any,Any)),tuple)
           ]
             where
               utf16Text bl = (False,pPrint . blob UTF16LEEncoding . blobBytes $ bl)
               utf8Text bl = (False,pPrint . blob UTF8Encoding . blobBytes $ bl)
               --utf8Text blob = (False,decText T.decodeUtf8 blob)
               -- decText dec = text . T.unpack . dec . blobBytes
               blobBytes b =let [_,bs] = valFields b in bytes bs
               bytes bs = let [_,vs] = valFields . head . valFields $ bs
                          in B.pack . map (fromIntegral . wrd_) . arrList $ vs

               bits8 (Value {valName = "Bits8", valFields = bs}) = bits bs
               bits7 (Value {valName = "Bits7", valFields = bs}) = bits bs
               bits = map bit
               bit v = let [b] = valBits v in b
               floatVal (Value {valName = "IEEE_754_binary32",valFields = [Value {valBits = [signVal]},Value {valName = "MostSignificantFirst", valFields = [expVal]},Value {valName = "MostSignificantFirst", valFields = [Value {valName = "Bits23", valFields = [frac1,frac2,frac3]}]}]}) =
                 let sign = fromIntegral $ fromEnum signVal -- then (-1::Int) else 1
                     exp = fromIntegral $ bitsVal $ bits8 expVal
                     fracBits = concat [[True],bits7 frac1,bits8 frac2,bits8 frac3]
                     frac = bitsVal fracBits
                     val = ((-1)**sign)*(fromIntegral frac / (2 ^ (length fracBits -1)))*(2**(exp-127))
                 in val --in error $ show (sign,exp,frac,val)

-- MSF bitsVal
-- bitsVal [True,False,True,False]
-- > 10
bitsVal :: [Bool] -> Int
bitsVal = fst . foldl (\(t,e) n -> (t+fromEnum n *2^e,e+1)) (0,0) . reverse

-- Used to match any type
data Any deriving (Generic,Model)
tAny = absType (Proxy:: Proxy Any)

match (TypeApp f1 a1) (TypeApp f2 a2) = match f1 f2 && match a1 a2
match t1 t2 | t2 == tAny = True
            | otherwise = t1 == t2

-- arr_ f elems = hsep $ text "Array" : [prettyList f elems]
--arr_ :: Pretty b => (a -> b) -> [a] -> Doc
--arr_ f elems = hsep [prettyList_ f elems]
arr_ = prettyList_

-- prettyList_ :: Pretty b => (a -> b) -> [a] -> Doc
-- prettyList_ f vs = pPrint $ map f vs
prettyList_ :: (a -> Doc) -> [a] -> Doc
prettyList_ f vs = prettyList $ map f vs

prettyTuple_ :: (a -> Doc) -> [a] -> Doc
prettyTuple_ f vs = prettyTuple $ map f vs

-- prettyTuple_ = prettyList

-- haskell Style
-- prettySeq sep1 sep2 f elems = char sep1 <> (hcat . intersperse (text ", ") . map f $ elems) <> char sep2
-- prettySeq sep1 sep2 f elems = char sep1 <> (hcat . intersperse (text ", ") . map f $ elems) <> char sep2
