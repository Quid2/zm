{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pretty instances for some basic Haskell types and for data type models
module ZM.Pretty (
  module Data.Model.Pretty,
  hex,
  unPrettyRef,
  prettyList,
  prettyTuple,
  text,
  txt,
  chr,
  str,
  hsep,
)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Model.Pretty
import Data.Model.Util
import Flat (UTF16Text (..), UTF8Text (..))

-- import           Data.Ord
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Word
import Numeric (readHex)
import Text.ParserCombinators.ReadP hiding (char)

-- import           Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import qualified Text.PrettyPrint.HughesPJClass as P

-- import           ZM.BLOB

import Data.Text (unpack)
import ZM.Model ()
import ZM.Pretty.Base
import ZM.Types

{- |
Convert the textual representation of a hash code to its equivalent value

>>> unPrettyRef "Kb53bec846608"
SHAKE128_48 181 59 236 132 102 8

>>> unPrettyRef "Kb53bec8466080000"
*** Exception: unPrettyRef: unknown code "b53bec8466080000"
...
-}
unPrettyRef :: String -> SHAKE128_48 a
-- unPrettyRef ('K':code) = let [k1,k2,k3,k4,k5,k6] = readHexCode code in SHAKE128_48 k1 k2 k3 k4 k5 k6
unPrettyRef ('K' : code) = case readHexCode code of
  [k1, k2, k3, k4, k5, k6] -> SHAKE128_48 k1 k2 k3 k4 k5 k6
  _ -> error $ "unPrettyRef: unknown code " ++ show code
unPrettyRef code = error $ "unPrettyRef: unknown code " ++ show code

{- | Display a list of Docs, as a tuple with spaced elements

>>> prettyTuple (map pPrint [11,22,33::Word8])
(11, 22, 33)
-}
prettyTuple :: [Doc] -> Doc
prettyTuple = parens . fsep . punctuate comma

{- | Display a list of Docs, with spaced elements
>>> prettyList (map pPrint [11,22,33::Word8])
[11, 22, 33]
-}
prettyList :: [Doc] -> Doc
-- prettyList = brackets . hcat . punctuate comma
prettyList = brackets . fsep . punctuate comma

instance Pretty TypedDecodeException where
  pPrint (UnknownMetaModel m) = text "Unknown meta model" P.<> pPrint m
  pPrint (WrongType e a) =
    let et = prettyShow e
        at = prettyShow a
     in text
          . unwords
          $ ["Was expecting type:\n", et, "\n\nBut the data has type:\n", at]
  pPrint (DecodeError e) = pPrint (show e)

-- TODO: merge with similar code in `model` package
instance Pretty AbsTypeModel where
  pPrint (TypeModel t e) =
    spacedP
      [ -- , vcat [pPrint t P.<> text ":", pPrint (e, t)]
        -- TODO: display as LocalName.AbsName
        mconcat [text "Type: ", pPrint (e, t), text " (", pPrint t, text ")"]
      , -- ,vcat [pPrint t <> text ":",pPrint (declName <$> solveAll e t)]
        text "Environment:"
      , pPrint e
      ]

instance {-# OVERLAPS #-} Pretty AbsEnv where
  -- Previous
  -- pPrint e = vspacedP . map (\(ref,adt) -> vcat [pPrint ref <> text ":",pPrint . CompactPretty $ (e,adt)]) . sortedEnv $ e

  pPrint :: AbsEnv -> Doc
  pPrint e =
    spacedP
      . map (\(ref, adt) -> pPrint (refADT e ref adt) P.<> char ';')
      . sortedEnv
      $ e

sortedEnv ::
  M.Map a (ADT Identifier Identifier (ADTRef AbsRef)) ->
  [(a, ADT Identifier Identifier (ADTRef AbsRef))]
sortedEnv = sortOn snd . M.assocs

refADT ::
  ( Pretty p
  , Convertible name String
  , Show k
  , Ord k
  , Pretty k
  , Convertible a String
  ) =>
  M.Map k (ADT a consName1 ref) ->
  p ->
  ADT name consName2 (ADTRef k) ->
  ADT QualName consName2 (TypeRef QualName)
-- refADT env ref adt =
--   let name = fullName ref adt
--   in ADT name (declNumParameters adt) ((solveS name <$>) <$> declCons adt)
--    where -- solveS _ (Var n) = TypVar n
--          -- solveS _ (Ext k) = TypRef . fullName k . solve k $ env
--          -- solveS name Rec  = TypRef name
--          solveS name = (fullName <$>) . toTypeRef name
--          fullName ref adt = QualName "" (prettyShow ref) (convert $ declName adt)

refADT env ref adt =
  let name = fullName ref adt
   in ADT name (declNumParameters adt) ((solveS name <$>) <$> declCons adt)
 where
  solveS _ (Var n) = TypVar n
  solveS _ (Ext k) = TypRef . fullName k . solve k $ env
  solveS name Rec = TypRef name
  -- fullName ref adt = QualName "" (prettyShow ref) (convert $ declName adt)
  fullName ref adt = QualName "" (convert $ declName adt) (prettyShow ref)

instance {-# OVERLAPS #-} Pretty (AbsEnv, AbsType) where
  pPrint :: (AbsEnv, AbsType) -> Doc
  pPrint (env, t) = pPrint (declName <$> solveAll env t)

instance {-# OVERLAPS #-} Pretty (AbsEnv, AbsADT) where
  pPrint :: (AbsEnv, AbsADT) -> Doc
  pPrint (env, adt) =
    pPrint $ substAbsADT (\ref -> declName $ solve ref env) adt

{- | Convert references in an absolute definition to their textual form (useful for display)
 stringADT :: AbsEnv -> AbsADT -> ADT Identifier Identifier (TypeRef Identifier)
 stringADT env adt =
   let name = declName adt
   in ADT name (declNumParameters adt) ((solveS name <$>) <$> declCons adt)
    where solveS _ (Var n) = TypVar n
          solveS _ (Ext k) = TypRef . declName . solve k $ env
          solveS name Rec  = TypRef name
-}

-- strADT :: AbsEnv -> AbsADT -> ADT Identifier Identifier (TypeRef Identifier)

-- stringADT :: AbsEnv -> AbsADT -> ADT Identifier Identifier (TypeRef Identifier)

-- stringADT = substADT (\env k -> declName $ solve k env)

-- stringADT = substAbsADT declName

-- substAbsADT f env adt = ((\ref -> f $ solve ref env) <$>) <$> (toTypeRef (absRef adt) <$> adt)

-- substADT k env adt = (k env <$>) <$> (toTypeRef (absRef adt) <$> adt)

instance Pretty Identifier where
  pPrint = text . convert

instance {-# OVERLAPS #-} (Pretty a) => Pretty (String, ADTRef a) where
  pPrint (_, Var v) = varP v
  pPrint (n, Rec) = text n
  pPrint (_, Ext r) = pPrint r

instance (Pretty a) => Pretty (ADTRef a) where
  pPrint (Var v) = varP v
  pPrint Rec = char '\x21AB'
  pPrint (Ext r) = pPrint r

readHexCode :: String -> [Word8]
readHexCode = readCode []

readCode :: [Word8] -> String -> [Word8]
readCode bs [] = reverse bs
readCode bs s = let (h, t) = splitAt 2 s in readCode (rdHex h : bs) t

{- | Read the last 2 characters as an hex value between 0 and FF

>>> rdHex ""
*** Exception: rdHex: cannot parse
...

>>> rdHex "3"
3

>>> rdHex "08"
8

>>> rdHex "FF07"
7
-}
rdHex :: String -> Word8
rdHex s = case readP_to_S (readS_to_P readHex) s of
  [(b, "")] -> b
  _ -> error $ "rdHex: cannot parse " ++ s

-- instance Pretty a => Pretty (Array a) where pPrint (Array vs) = text "Array" <+> pPrint vs
-- instance Pretty a => Pretty (Array a) where pPrint (Array vs) = pPrint vs
instance (Pretty a) => Pretty (S.Seq a) where
  pPrint = pPrint . toList

instance (Pretty a) => Pretty (NonEmptyList a) where
  pPrint = pPrint . toList

instance Pretty NoEncoding where
  pPrint = text . show

instance Pretty T.Text where
  pPrint = text . T.unpack
instance Pretty UTF8Text where
  pPrint (UTF8Text t) = pPrint t
instance Pretty UTF16Text where
  pPrint (UTF16Text t) = pPrint t

instance Pretty Word where
  pPrint = text . show
instance Pretty Word8 where
  pPrint = text . show
instance Pretty Word16 where
  pPrint = text . show
instance Pretty Word32 where
  pPrint = text . show
instance Pretty Word64 where
  pPrint = text . show
instance Pretty Int8 where
  pPrint = text . show
instance Pretty Int16 where
  pPrint = text . show
instance Pretty Int32 where
  pPrint = text . show
instance Pretty Int64 where
  pPrint = text . show

instance Pretty B.ByteString where
  pPrint = pPrint . B.unpack
instance Pretty L.ByteString where
  pPrint = pPrint . L.unpack
instance Pretty SBS.ShortByteString where
  pPrint = pPrint . SBS.unpack

instance (Pretty a, Pretty b) => Pretty (M.Map a b) where
  pPrint m = text "Map" <+> pPrint (M.assocs m)

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

pPrint0 :: (Pretty a) => PrettyLevel -> a -> Doc
pPrint0 l = pPrintPrec l 0

-- instance (Pretty a,Pretty l) => Pretty (Label a l) where
--    pPrint (Label a Nothing)  = pPrint a
--    pPrint (Label _ (Just l)) = pPrint l

txt :: T.Text -> Doc
txt = text . unpack

chr :: Char -> Doc
chr = text . (: "")

str :: String -> Doc
str = text