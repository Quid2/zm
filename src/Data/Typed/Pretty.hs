{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
module Data.Typed.Pretty(
  -- Pretty(..)
  -- ,module Text.PrettyPrint.HughesPJClass
  module Data.Model.Pretty
  ,hex,unPrettyRef,prettyList,prettyTuple--,prettyWords
  ) where

import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           Data.Char                      (chr)
import           Data.Flat.Instances
import           Data.Foldable                  (toList)
import           Data.Int
import           Data.List
import qualified Data.ListLike.String           as L
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Model.Class
import           Data.Model.Pretty
import           Data.Ord
import qualified Data.Text                      as T
import           Data.Typed.Class
import           Data.Typed.Instances
import qualified Data.Typed.PrimTypes           as P
import           Data.Typed.Transform
import           Data.Typed.Types
import           Data.Word
import           Data.ZigZag
import           Numeric                        (readHex)
import           Text.ParserCombinators.ReadP   hiding (char)
import           Text.PrettyPrint.HughesPJClass
import           Text.Printf

-- Display as corresponding Haskell value
instance Pretty Val where
     pPrintPrec (PrettyLevel lev) prec = pp 0
       where -- TODO: fix precedence
         pp l v@(Val t n bs vs) = let (complex,doc) = sh l v
                                   in (if complex && l>0 then parens else id) (hsep $ (if null bs || lev == 0 then empty else (text (map (\b -> if b then '1' else '0') bs) <> char ':')) : doc) -- : if lev == 0 then empty else )

         -- pp l v@(Val n bs vs) = let (complex,doc) = sh l v
         --                        in (if complex && l>0 then parens else id) (hsep $ (if null bs || lev == 0 then empty else (text (map (\b -> if b then '1' else '0') bs) <> char ':')) : doc)

         sh l v@(Val t n _ vs) = case prettyPrinter t of
           Nothing -> (not (null vs),txt n : map (pp (l+1)) vs)
           -- Nothing -> (not (null vs),[txt n] ++ map (pp (l+1)) vs ++ [text ":: ",text . show $ t])
           Just pr -> let (c,d) = pr v in (c,[d])

         -- sh l v@(Val "BLOB" _ [Val "UTF8Encoding" _ [],Val "PreAligned" _ [_,cs]]) | lev == 0 = sh l cs
         -- sh l v@(Val "Cons" _ (Val "Char" _ _:_)) | lev == 0 = (False,[char '"' <> (text . map ch . valList $ v) <> char '"'])
         -- sh l v@(Val "Cons" _ _)                  | lev == 0 = (False,[ar (valList v)])
         -- sh l v@(Val "Nil" _ [])                  | lev == 0 = (False,[text "[]"])
         -- sh _ v@(Val "Char" _ _)                  | lev == 0 = (False,[text ['\'',ch v,'\'']])
         -- sh _ v@(Val (T.unpack -> 'V':n) _ _)     | lev == 0 = (False,[int (read n :: Int)])
         -- sh _ v@(Val "A0" _ _)                    | lev == 0 = (True,[text "Array []"])
         -- sh _ v@(Val (T.unpack -> 'A':n) _ vs)    | lev == 0 = (True,[arr (init vs)]) -- FIX
         -- sh _ v@(Val n _ vs) | lev == 0 && T.isPrefixOf "Tuple" n = (False,[tup vs])
         -- sh l (Val n bs vs)                       = (not (null vs),txt n : map (pp (l+1)) vs)

         -- ch (Val "Char" _ [Val "Word32" _ [Val "Elem" _ [Val (T.unpack -> 'V':n) _ []]]]) = chr (read n::Int)
         -- ch (Val _ _ _ [Val _ _ _ [Val _ _ _ [Val _ (strName -> 'V':n) _ []]]]) = chr (read n::Int)
         ch (Val _ _ _ [Val _ _ _ [Val _ _ _ [Val _ _ _ [Val _ _ _ [Val _ _ _ [Val _ (strName -> 'V':n) _ []]]]]]]) = chr (read n::Int)
         ch v = error (show v)

         -- wrd_ (Val (T.unpack -> 'V':n) _ _) = read n :: Int
         wrd_ (Val _ (strName -> 'V':n) _ _) = read n :: Int
         wrd_ v = error (unwords ["wrd_",show v])

         wrd = int . wrd_

         wrd2_ (Val _ _ _ [Val _ (strName -> 'V':n) _ _]) = read n :: Int
         wrd2_ v = error (unwords ["wrd2_",show v])

         -- i :: forall a . Proxy a -> (AbsType, Val -> (Bool, Doc))
         -- i p = (absType p,\v -> (False,int . (zzDecode::(Bits a,Integral a) => Word64 -> a) . wl_ . p0 . p0 $ v))

         wl = int . wl_

         --wl_ (Val _ _ _ [vl]) = fromIntegral . fst . foldl (\(t,e) n -> (t+n*2^e,e+7)) (0,0) . map wrd_ . neList $ vl
         wl_ (Val _ _ _ [Val _ _ _ [Val _ _ _ [vl]]]) = fromIntegral . fst . foldl (\(t,e) n -> (t+n*2^e,e+7)) (0,0) . map wrd2_ . neList $ vl
         wl_ v = error (unwords ["wl_",show v])

         -- valList (Val "Cons" _ [h,t]) = h:valList t
         -- valList (Val "Nil" _ []) = []
         valList (Val _ "Cons" _ [h,t]) = h:valList t
         valList (Val _ "Nil"  _ []) = []
         valList v = error (unwords ["valList",show v])

         neList (Val _ "Cons" _ [h,t]) = h:neList t
         neList (Val _ "Elem"  _ [e]) = [e]
         neList v = error (unwords ["neList",show v])

         arrList v@(Val _ "A0" _ []) = []
         arrList v@(Val _ (strName -> 'A':n) _ vs) | length vs == read n + 1 = init vs ++ arrList (last vs)

         arr = arr_ (pp 0)
         ar = prettyList (pp 0)

         tup = prettyTuple_ (pp 0)

         prettyPrinter t = listToMaybe . map snd . filter fst . map (\(t2,p) -> (match t t2,p)) $ [
           (absType (Proxy::Proxy Unit),\_ -> (False,text "()"))
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
           ,(absType (Proxy::Proxy Char),\v -> (False,text ['\'',ch v,'\'']))
           ,(absType (Proxy::Proxy (P.List Char)),\v -> (False,char '"' <> (text . map ch . valList $ v) <> char '"'))
           ,(absType (Proxy::Proxy (P.List Any)),\v -> (False,ar (valList v)))
           ,(absType (Proxy::Proxy (NonEmptyList Any)),\v -> (False,ar (neList v)))
           ,(absType (Proxy::Proxy (P.Array Any)),\v -> (True,arr (arrList v)))
           ,(absType (Proxy::Proxy (BLOB UTF8Encoding)),\(Val _ _ _ [_,Val _ _ _ [_,vs]]) -> (False,text . map (chr . wrd_) . arrList $ vs))
           ,(absType (Proxy::Proxy (Tuple2 Any Any)),tuple)
           ,(absType (Proxy::Proxy (Tuple3 Any Any Any)),tuple)
           ,(absType (Proxy::Proxy (Tuple4 Any Any Any Any)),tuple)
           ,(absType (Proxy::Proxy (Tuple5 Any Any Any Any Any)),tuple)
           ,(absType (Proxy::Proxy (Tuple6 Any Any Any Any Any Any)),tuple)
           ,(absType (Proxy::Proxy (Tuple7 Any Any Any Any Any Any Any)),tuple)
           ,(absType (Proxy::Proxy (Tuple8 Any Any Any Any Any Any Any Any)),tuple)
           ,(absType (Proxy::Proxy (Tuple9 Any Any Any Any Any Any Any Any Any)),tuple)
           ]
         p0 (Val _ _ _ [v]) = v
         tuple (Val _ _ _ vs) = (False,tup vs)

-- Used to match any type
data Any deriving (Generic,Model)
tAny = absType (Proxy:: Proxy Any)

match (TypeApp f1 a1) (TypeApp f2 a2) = match f1 f2 && match a1 a2
match t1 t2 | t2 == tAny = True
            | otherwise = t1 == t2

arr_ :: (a -> Doc) -> [a] -> Doc
arr_ f elems = hsep $ text "Array" : [prettyList f elems]

prettyList :: (a -> Doc) -> [a] -> Doc
prettyList = prettySeq '[' ']'

prettyTuple = prettySeq '(' ')' id

prettyTuple_ = prettySeq '(' ')'

prettySeq sep1 sep2 f elems = char sep1 <> (hcat . intersperse (char ',') . map f $ elems) <> char sep2

-- x = prettyShow $ B.pack [11,22]
y = prettyShow [11,22::Word8]
z = prettyShow 'a'

-- See also flat/Data.Flat.Run
--instance Pretty B.ByteString where pPrint = arr_ (int . fromIntegral) . B.unpack
-- instance Pretty L.ByteString where pPrint = arr_ (int . fromIntegral) . L.unpack

instance Show a => Pretty (TypedValue a) where pPrint (TypedValue t v)= text (show v) <+> text "::" <+> pPrint t

instance Pretty AbsoluteType where
 pPrint (AbsoluteType e t) = vcat . (pPrint t <+> text "->" <+> pPrint (declName <$> solveF e t) :) . (text "" :) . (text "Data Types:" :) . map (\(h,adt) -> pPrint h <+> text "->" <+> pPrint (e,adt)) $ M.assocs e

instance Pretty AbsRef where pPrint (AbsRef sha3) = pPrint sha3

instance {-# OVERLAPS #-} Pretty ADTEnv where
 -- pPrint e = vcat . map (\(h,adt) -> pPrint h <+> text "->" <+> pPrint (e,adt)) $ M.assocs e
 pPrint e = vspacedP . sortBy (comparing snd) . map (\(h,adt) -> (e,adt)) $ M.assocs e

instance {-# OVERLAPS #-} Pretty (ADTEnv,AbsADT) where
  -- pPrint (env,adt) = prettyADT "" '≡'. stringADT env $ adt
  -- pPrint (env,adt) = pPrint . CompactPretty . stringADT env $ adt
  -- if we use CompactPretty the ui won't display correctly
  pPrint (env,adt) = pPrint . stringADT env $ adt

instance Pretty LocalName where pPrint (LocalName n) = pPrint n

instance Pretty Identifier where pPrint = text . L.toString

instance Pretty a => Pretty (String,ADTRef a) where
   pPrint (_,Var v) = varP v
   pPrint (n,Rec) = text n
   pPrint (_,Ext r) = pPrint r

instance Pretty a => Pretty (ADTRef a) where
   pPrint (Var v) = varP v
   pPrint Rec = char '\x21AB'
   pPrint (Ext r) = pPrint r

-- instance Pretty (Ref a) where
--   pPrint (Verbatim bl) = char 'V' <> prettyNE bl -- pPrint bl
--   pPrint (Shake128 bl) = char 'H' <> prettyNE bl -- pPrint bl

-- instance Pretty SHA3_256_6 where pPrint (SHA3_256_6 bl) = char 'K' <> prettyNE bl -- pPrint bl
instance Pretty a => Pretty (SHA3_256_6 a) where pPrint (SHA3_256_6 k1 k2 k3 k4 k5 k6) = char 'S' <> prettyWords [k1,k2,k3,k4,k5,k6]

-- x =  unPrettyRef "H0a2d22a3"

 -- unPrettyRef ('H':code) = let [(bs,"")] = readP_to_S (many1 rdHex(readS_to_P readHex)) code
 --                          in Shake128 $ nonEmptyList $ bs

-- unPrettyRef :: String -> Ref a
-- unPrettyRef ('V':code) = Verbatim . nonEmptyList $ readHexCode code
-- unPrettyRef ('H':code) = Shake128 . nonEmptyList $ readHexCode code

unPrettyRef ('S':code) = let [k1,k2,k3,k4,k5,k6] = readHexCode code in SHA3_256_6 k1 k2 k3 k4 k5 k6

rdHex :: String -> Word8
rdHex s = let [(b,"")] = readP_to_S ((readS_to_P readHex)) s in b

readHexCode = readCode []

readCode bs [] = reverse bs
readCode bs s  = let (h,t) = splitAt 2 s
                  in readCode (rdHex h : bs) t

instance (Pretty a,Pretty l) => Pretty (Label a l) where
   pPrint (Label a Nothing) = pPrint a
   pPrint (Label _ (Just l)) = pPrint l

instance Pretty a => Pretty (NonEmptyList a) where pPrint = pPrint . toList

instance Pretty T.Text where pPrint = text . T.unpack

-- instance Pretty Word8 where pPrint = text . hex

--prettyWord = text . hex

prettyNE :: NonEmptyList Word8 -> Doc
-- prettyNE = cat . map pPrint . toList
prettyNE = prettyWords . toList

prettyWords = text . concatMap hex

hex = printf "%02x"

