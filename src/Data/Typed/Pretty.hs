{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- |Pretty instances for some basic Haskell types and for data type models
module Data.Typed.Pretty (
    module Data.Model.Pretty,
    hex,
    unPrettyRef,
    prettyList,
    prettyTuple,
    ) where

import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import qualified Data.ByteString.Short          as SBS
import           Data.Flat                      (UTF16Text (..), UTF8Text (..))
import           Data.Foldable                  (toList)
import           Data.Int
import           Data.List
import qualified Data.ListLike.String           as L
import qualified Data.Map                       as M
import           Data.Model.Pretty
import           Data.Ord
import qualified Data.Sequence                  as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Typed.BLOB
import           Data.Typed.Model               ()
import           Data.Typed.Transform
import           Data.Typed.Types
import           Data.Word
import           Numeric                        (readHex)
import           Text.ParserCombinators.ReadP   hiding (char)
import           Text.PrettyPrint.HughesPJClass
import           Text.Printf

-- |Convert the textual representation of a hash code to its equivalent value
unPrettyRef :: String -> SHA3_256_6 a
unPrettyRef ('S':code) = let [k1,k2,k3,k4,k5,k6] = readHexCode code in SHA3_256_6 k1 k2 k3 k4 k5 k6
unPrettyRef code = error $ "unPrettyRef: unknown code " ++ show code
-- unPrettyRef ('K':code) = let [k1,k2,k3,k4,k5,k6] = readHexCode code in SHAKE128_48 k1 k2 k3 k4 k5 k6

-- |Display a Word in hexadecimal format
hex :: Word8 -> String
hex = printf "%02x"

-- |Display a list of Docs, as a tuple with spaced elements
-- 
-- >>> prettyTuple (map pPrint [11,22,33::Word8])
-- (11, 22, 33)
prettyTuple :: [Doc] -> Doc
prettyTuple = parens . fsep . punctuate comma

-- |Display a list of Docs, with spaced elements
-- 
-- >>> prettyList (map pPrint [11,22,33::Word8])
-- [11, 22, 33]
prettyList :: [Doc] -> Doc
--prettyList = brackets . hcat . punctuate comma
prettyList = brackets . fsep . punctuate comma

instance Pretty TypedDecodeException where
  pPrint (UnknownMetaModel m) = text "Unknown meta model" <> pPrint m
  pPrint (WrongType e a) = let et  = prettyShow e
                               at = prettyShow a
                           in text . unwords $ ["Was expecting type:\n",et,"\n\nBut the data has type:\n",at]
  pPrint (DecodeError e) = pPrint (show e)

instance Show a => Pretty (TypedValue a) where pPrint (TypedValue t v)= text (show v) <+> text "::" <+> pPrint t

-- TODO: merge with similar code in `model` package
instance Pretty AbsTypeModel where
  pPrint (TypeModel t e) = vspacedP [
    text "Type:"
    ,vcat [pPrint t <> text ":",pPrint (e,t)]
    -- ,vcat [pPrint t <> text ":",pPrint (declName <$> solveAll e t)]
    ,text "Environment:"
    ,pPrint e
    ]

instance {-# OVERLAPS #-} Pretty AbsEnv where
 -- pPrint e = vcat . map (\(h,adt) -> pPrint h <+> text "->" <+> pPrint (e,adt)) $ M.assocs e
 pPrint e = vspacedP . map (\(ref,adt) -> vcat [pPrint ref <> text ":",pPrint . CompactPretty $ (e,adt)]) . sortBy (comparing snd) $ M.assocs e
 --pPrint e = vspacedP . sortBy (comparing snd) . map (\(h,adt) -> (e,adt)) $ M.assocs e

instance {-# OVERLAPS #-} Pretty (AbsEnv,AbsType) where
  pPrint (env,t) = pPrint (declName <$> solveAll env t)

instance {-# OVERLAPS #-} Pretty (AbsEnv,AbsADT) where
  pPrint (env,adt) = pPrint . stringADT env $ adt

instance Pretty Identifier where pPrint = text . L.toString

instance {-# OVERLAPS #-} Pretty a => Pretty (String,ADTRef a) where
   pPrint (_,Var v) = varP v
   pPrint (n,Rec)   = text n
   pPrint (_,Ext r) = pPrint r

instance Pretty a => Pretty (ADTRef a) where
   pPrint (Var v) = varP v
   pPrint Rec     = char '\x21AB'
   pPrint (Ext r) = pPrint r

instance Pretty AbsRef where pPrint (AbsRef sha3) = pPrint sha3

instance Pretty a => Pretty (SHA3_256_6 a) where pPrint (SHA3_256_6 k1 k2 k3 k4 k5 k6) = char 'S' <> prettyWords [k1,k2,k3,k4,k5,k6]

instance Pretty a => Pretty (SHAKE128_48 a) where pPrint (SHAKE128_48 k1 k2 k3 k4 k5 k6) = char 'K' <> prettyWords [k1,k2,k3,k4,k5,k6]

--instance Pretty a => Pretty (SHAKE_256_6 a) where pPrint (SHA3_256_6 k1 k2 k3 k4 k5 k6) = char 'S' <> prettyWords [k1,k2,k3,k4,k5,k6]

prettyWords :: [Word8] -> Doc
prettyWords = text . concatMap hex

readHexCode :: String -> [Word8]
readHexCode = readCode []

readCode :: [Word8] -> String -> [Word8]
readCode bs [] = reverse bs
readCode bs s  = let (h,t) = splitAt 2 s
                  in readCode (rdHex h : bs) t

rdHex :: String -> Word8
rdHex s = let [(b,"")] = readP_to_S (readS_to_P readHex) s in b
-- instance Pretty a => Pretty (Array a) where pPrint (Array vs) = text "Array" <+> pPrint vs
-- instance Pretty a => Pretty (Array a) where pPrint (Array vs) = pPrint vs
instance Pretty a => Pretty (S.Seq a) where pPrint = pPrint . toList

instance Pretty a => Pretty (NonEmptyList a) where pPrint = pPrint . toList

instance (Pretty a,Pretty l) => Pretty (Label a l) where
   pPrint (Label a Nothing)  = pPrint a
   pPrint (Label _ (Just l)) = pPrint l

instance Pretty NoEncoding where pPrint = text . show

instance Pretty encoding => Pretty (BLOB encoding) where pPrint (BLOB enc bs) = text "BLOB" <+> pPrint enc <+> pPrint bs

instance {-# OVERLAPS #-} Pretty (BLOB UTF8Encoding) where pPrint = pPrint . T.decodeUtf8 . unblob

instance {-# OVERLAPS #-} Pretty (BLOB UTF16LEEncoding) where pPrint = pPrint . T.decodeUtf16LE . unblob

instance Pretty T.Text where pPrint = text . T.unpack
instance Pretty UTF8Text where pPrint (UTF8Text t)= pPrint t
instance Pretty UTF16Text where pPrint (UTF16Text t)= pPrint t

instance Pretty Word where pPrint = text . show
instance Pretty Word8 where pPrint = text . show
instance Pretty Word16 where pPrint = text . show
instance Pretty Word32 where pPrint = text . show
instance Pretty Word64 where pPrint = text . show
instance Pretty Int8 where pPrint = text . show
instance Pretty Int16 where pPrint = text . show
instance Pretty Int32 where pPrint = text . show
instance Pretty Int64 where pPrint = text . show

instance Pretty B.ByteString where pPrint = pPrint . B.unpack
instance Pretty L.ByteString where pPrint = pPrint . L.unpack
instance Pretty SBS.ShortByteString where pPrint = pPrint . SBS.unpack

instance (Pretty a,Pretty b) => Pretty (M.Map a b) where pPrint m = text "Map" <+> pPrint (M.assocs m)

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h, Pretty i) => Pretty (a, b, c, d, e, f, g, h, i) where
  pPrintPrec l _ (a, b, c, d, e, f, g, h, i) =
    prettyTuple
      [ pPrint0 l a
      , pPrint0 l b
      , pPrint0 l c
      , pPrint0 l d
      , pPrint0 l e
      , pPrint0 l f
      , pPrint0 l g
      , pPrint0 l h
      , pPrint0 l i
      ]

pPrint0 :: Pretty a => PrettyLevel -> a -> Doc
pPrint0 l = pPrintPrec l 0

