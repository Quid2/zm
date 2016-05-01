{-# LANGUAGE FlexibleInstances #-}
module Data.Typed.Pretty(
  -- Pretty(..)
  -- ,module Text.PrettyPrint.HughesPJClass
  module Data.Model.Pretty
  ,hex,unPrettyRef
  ) where

import           Data.Char                      (chr)
import           Data.Foldable                  (toList)
import           Data.List
import           Data.Model.Pretty
import           Data.Typed.Transform
import           Data.Typed.Types
import           Data.Word
import           Numeric                        (readHex)
import           Text.ParserCombinators.ReadP   hiding (char)
import           Text.PrettyPrint.HughesPJClass
import           Text.Printf

instance Pretty Val where
     pPrintPrec (PrettyLevel lev) prec = pp lev 0
       where -- TODO: fix precedence
         pp lev l v@(Val n bs vs) = (if l>0 && not (null vs) then parens else id) (hsep $ (if null bs || lev == 0 then empty else (text (map (\b -> if b then '1' else '0') bs) <> char ':')) : sh lev l v)
         sh 0 l v@(Val "Cons" _ (Val "Char" _ _:_)) = [hcat $ (char '"' : (concatMap (sh 0 l) . valList $ v))++[char '"']]
         sh 0 l v@(Val "Cons" _ _) = (char '[' : intersperse (text ",") (concatMap (sh 0 l) . valList $ v))++[char ']']
         sh 0 _ (Val "Char" _ [Val "Word32" _ [Val "Elem" _ [Val ('V':n) _ []]]]) = [char (chr (read n::Int))]
         sh lev l (Val n bs vs) = text n : map (pp lev (l+1)) vs
         valList (Val "Cons" _ [h,t]) = h:valList t
         valList (Val "Nil" _ []) = []

instance {-# OVERLAPS #-} Pretty (ADTEnv,AbsADT) where
   pPrint (env,adt) = pPrint . stringADT env $ adt

instance {-# OVERLAPS #-} Pretty (MutualADTEnv,MutualAbsADT) where
   pPrint (env,adt) = vcat . intersperse (text "") . map pPrint . stringADTs env $ adt

instance Pretty LocalName where pPrint (LocalName n) = text n

instance Pretty (String,ADTRef) where
   pPrint (_,Var v) = varP v
   pPrint (n,Rec) = text n
   pPrint (_,Ext r) = pPrint r

instance Pretty ADTRef where
   pPrint (Var v) = varP v
   pPrint Rec = char '\x21AB'
   pPrint (Ext r) = pPrint r

instance Pretty MutualADTRef where
   pPrint (MVar v) = varP v
   pPrint (MRec s) = text s
   pPrint (MExt r) = pPrint r

instance Pretty (Ref a) where
  pPrint (Verbatim bl) = char 'V' <> prettyNE bl -- pPrint bl
  pPrint (Shake128 bl) = char 'H' <> prettyNE bl -- pPrint bl

-- x =  unPrettyRef "H0a2d22a3"

 -- unPrettyRef ('H':code) = let [(bs,"")] = readP_to_S (many1 rdHex(readS_to_P readHex)) code
 --                          in Shake128 $ nonEmptyList $ bs

unPrettyRef :: String -> Ref a
unPrettyRef ('V':code) = Verbatim . nonEmptyList $ readHexCode code
unPrettyRef ('H':code) = Shake128 . nonEmptyList $ readHexCode code

rdHex :: String -> Word8
rdHex s = let [(b,"")] = readP_to_S ((readS_to_P readHex)) s in b

readHexCode = readCode []

readCode bs [] = reverse bs
readCode bs s  = let (h,t) = splitAt 2 s
                  in readCode (rdHex h : bs) t

instance Pretty a => Pretty (Label a) where
   pPrint (Label a Nothing) = pPrint a
   pPrint (Label a (Just l)) = text l

instance Pretty a => Pretty (NonEmptyList a) where pPrint = pPrint . toList

instance Pretty Word8 where pPrint = text . hex

prettyNE :: NonEmptyList Word8 -> Doc
prettyNE = cat . map pPrint . toList

hex = printf "%02x"
