{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass     ,NoMonomorphismRestriction #-}
module Data.Typed.Value(
  Value(..)
  ) where

import           Data.Char                      (chr)
import           Data.Flat
import qualified Data.Flat.Instances            as P
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Model.Class
import           Data.Model.Pretty
import           Data.Typed.Class
import           Data.Typed.Model               ()
import           Data.Typed.Types
import           Data.Word
import           Data.ZigZag
import           Text.PrettyPrint.HughesPJClass
import           Data.Typed.Pretty

-- Generic value (used for dynamic decoding)
data Value = Value {valType::AbsType -- Type
               ,valName::String   -- Constructor name (duplicate info if we have abstype)
               ,valBits::[Bool] -- Bit encoding/constructor id
                 -- TODO: add field names (same info present in abstype)
               ,valFields::[Value]  -- Values to which the constructor is applied, if any
               } deriving  (Eq,Ord,Show,NFData, Generic, Flat)

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

         wrd_ (Value _ ('V':n) _ _) = read n :: Int
         wrd_ v                              = error (unwords ["wrd_",show v])

         wrd = int . wrd_

         wrd2_ (Value _ _ _ [Value _ ('V':n) _ _]) = read n :: Int
         wrd2_ v = error (unwords ["wrd2_",show v])

         -- i :: forall a . Proxy a -> (AbsType, Value -> (Bool, Doc))
         -- i p = (absType p,\v -> (False,int . (zzDecode::(Bits a,Integral a) => Word64 -> a) . wl_ . p0 . p0 $ v))

         wl = int . wl_

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
           --,(absType (Proxy::Proxy Char),\v -> (False,text ['\'',ch v,'\'']))
           ,(absType (Proxy::Proxy Char),\v -> (False,pPrint (ch v)))
           ,(absType (Proxy::Proxy [Char]),\v -> (False,pPrint . map ch . valList $ v))
           --,(absType (Proxy::Proxy ([Char])),\v -> (False,char '"' <> (text . map ch . valList $ v) <> char '"'))
           ,(absType (Proxy::Proxy [Any]),\v -> (False,ar (valList v)))
           ,(absType (Proxy::Proxy (NonEmptyList Any)),\v -> (False,ar (neList v)))
           ,(absType (Proxy::Proxy (P.Array Char)),\v -> (False,pPrint . map ch . arrList $ v))
           --,(absType (Proxy::Proxy (P.Array Any)),\v -> (True,arr (arrList v)))
           ,(absType (Proxy::Proxy (P.Array Any)),\v -> (False,arr (arrList v)))
           ,(absType (Proxy::Proxy (BLOB UTF8Encoding)),\(Value _ _ _ [_,Value _ _ _ [_,vs]]) -> (False,text . map (chr . wrd_) . arrList $ vs))
           ,(absType (Proxy::Proxy (Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any,Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any,Any,Any,Any,Any)),tuple)
           ,(absType (Proxy::Proxy (Any,Any,Any,Any,Any,Any,Any,Any,Any)),tuple)
           ]

-- Used to match any type
data Any deriving (Generic,Model)
tAny = absType (Proxy:: Proxy Any)

match (TypeApp f1 a1) (TypeApp f2 a2) = match f1 f2 && match a1 a2
match t1 t2 | t2 == tAny = True
            | otherwise = t1 == t2

-- arr_ f elems = hsep $ text "Array" : [prettyList f elems]
arr_ f elems = hsep $ [prettyList_ f elems]

prettyList_ f vs = pPrint $ map f vs

prettyTuple_ f vs = prettyTuple $ map f vs

-- prettyTuple_ = prettyList

-- haskell Style
-- prettySeq sep1 sep2 f elems = char sep1 <> (hcat . intersperse (text ", ") . map f $ elems) <> char sep2
-- prettySeq sep1 sep2 f elems = char sep1 <> (hcat . intersperse (text ", ") . map f $ elems) <> char sep2
