{-# LANGUAGE FlexibleInstances #-}
module Data.Typed.Pretty(
  Pretty(..)
  ,module Text.PrettyPrint.HughesPJClass
  ,hex,unPrettyRef
  ) where

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
     pPrint = pp 0
       where
         pp l (Val n bs vs) = (if l>0 && not (null vs) then parens else id) (hsep $ (if null bs then empty else (text (map (\b -> if b then '1' else '0') bs) <> char ':')) <> text n : map (pp (l+1)) vs)

instance {-# OVERLAPS #-} Pretty (ADTEnv,AbsADT) where pPrint (env,adt) = vcat . intersperse (text "") . map pPrint . stringADTs env $ adt

instance Pretty LocalName where pPrint (LocalName n) = text n

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
