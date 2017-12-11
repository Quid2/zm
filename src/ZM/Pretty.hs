{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- |Pretty instances for some basic Haskell types and for data type models
module ZM.Pretty (
    module Data.Model.Pretty,
    hex,
    unPrettyRef,
    prettyList,
    prettyTuple
    ) where

import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import qualified Data.ByteString.Short          as SBS
import           Data.Flat                      (UTF16Text (..), UTF8Text (..))
import           Data.Foldable                  (toList)
import           Data.Int
import           Data.List
import qualified Data.Map                       as M
import           Data.Model.Pretty
import           Data.Model.Util
import           Data.Ord
import qualified Data.Sequence                  as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Word
import           Numeric                        (readHex)
import           Text.ParserCombinators.ReadP   hiding (char)
import           Text.PrettyPrint.HughesPJClass
import           ZM.BLOB
import           ZM.Model                       ()
import           ZM.Pretty.Base
import           ZM.Types

-- |Convert the textual representation of a hash code to its equivalent value
--
-- >>> unPrettyRef "Kb53bec846608"
-- SHAKE128_48 181 59 236 132 102 8
unPrettyRef :: String -> SHAKE128_48 a
unPrettyRef ('K':code) = let [k1,k2,k3,k4,k5,k6] = readHexCode code in SHAKE128_48 k1 k2 k3 k4 k5 k6

--unPrettyRef :: String -> SHA3_256_6 a
--unPrettyRef ('S':code) = let [k1,k2,k3,k4,k5,k6] = readHexCode code in SHA3_256_6 k1 k2 k3 k4 k5 k6
unPrettyRef code = error $ "unPrettyRef: unknown code " ++ show code

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
 -- Previous
 -- pPrint e = vspacedP . map (\(ref,adt) -> vcat [pPrint ref <> text ":",pPrint . CompactPretty $ (e,adt)]) . sortedEnv $ e

  pPrint e = vspacedP . map (\(ref,adt) -> pPrint (refADT e ref adt) <> char ';') . sortedEnv $ e

sortedEnv :: M.Map a (ADT Identifier Identifier (ADTRef AbsRef)) -> [(a, ADT Identifier Identifier (ADTRef AbsRef))]
sortedEnv = sortBy (comparing snd) . M.assocs

refADT :: (Pretty p, Convertible name String, Show k, Ord k, Pretty k, Convertible a String) => M.Map k (ADT a consName1 ref) -> p -> ADT name consName2 (ADTRef k) -> ADT QualName consName2 (TypeRef QualName)
refADT env ref adt =
  let name = fullName ref adt
  in ADT name (declNumParameters adt) ((solveS name <$>) <$> declCons adt)
   where solveS _ (Var n) = TypVar n
         solveS _ (Ext k) = TypRef . fullName k . solve k $ env
         solveS name Rec  = TypRef name
         fullName ref adt = QualName "" (prettyShow ref) (convert $ declName adt)

instance {-# OVERLAPS #-} Pretty (AbsEnv,AbsType) where
  pPrint (env,t) = pPrint (declName <$> solveAll env t)

instance {-# OVERLAPS #-} Pretty (AbsEnv,AbsADT) where
  pPrint (env,adt) = pPrint . stringADT env $ adt

-- |Convert references in an absolute definition to their textual form (useful for display)
stringADT :: AbsEnv -> AbsADT -> ADT Identifier Identifier (TypeRef Identifier)
stringADT env adt =
  let name = declName adt
  in ADT name (declNumParameters adt) ((solveS name <$>) <$> declCons adt)
   where solveS _ (Var n) = TypVar n
         solveS _ (Ext k) = TypRef . declName . solve k $ env
         solveS name Rec  = TypRef name

instance Pretty Identifier where pPrint = text . convert

instance {-# OVERLAPS #-} Pretty a => Pretty (String,ADTRef a) where
   pPrint (_,Var v) = varP v
   pPrint (n,Rec)   = text n
   pPrint (_,Ext r) = pPrint r

instance Pretty a => Pretty (ADTRef a) where
   pPrint (Var v) = varP v
   pPrint Rec     = char '\x21AB'
   pPrint (Ext r) = pPrint r


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

