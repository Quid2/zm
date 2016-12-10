{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Data.Typed.Pretty(
  module Data.Model.Pretty
  ,CompactPretty(..)
  ,hex,unPrettyRef
  -- ,prettyList,
  ,prettyTuple
  ) where

import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           Data.Flat.Instances            (Array (..))
import           Data.Foldable                  (toList)
import           Data.Int
import           Data.List
import qualified Data.ListLike.String           as L
import qualified Data.Map                       as M
import           Data.Model.Pretty
import           Data.Ord
import qualified Data.Text                      as T
import           Data.Typed.Model               ()
import           Data.Typed.Transform
import           Data.Typed.Types
import           Data.Word
import           Numeric                        (readHex)
import           Text.ParserCombinators.ReadP   hiding (char)
import           Text.PrettyPrint.HughesPJClass
import           Text.Printf

-- |Compact representation: a value enveloped in CompactPretty will have only its first lines displayed
data CompactPretty a = CompactPretty a

instance Pretty a => Pretty (CompactPretty a) where pPrint (CompactPretty a) = text . shorter . prettyShow $ a

shorter :: String -> String
shorter s =
   let ln = lines s
       l = length ln
   in if l > 11
      then unlines $ take 5 ln ++ ["..."]  ++ drop (l-5) ln
      else s

instance Show a => Pretty (TypedValue a) where pPrint (TypedValue t v)= text (show v) <+> text "::" <+> pPrint t

-- TODO: merge with similar code in `model` package
instance Pretty AbsTypeModel where
  pPrint (TypeModel t e) = vcat . (pPrint t <+> text "->" <+> pPrint (declName <$> solveAll e t) :) . (text "" :) . (text "Data Types:" :) . map (\(h,adt) -> pPrint h <+> text "->" <+> pPrint (e,adt)) $ M.assocs e

instance {-# OVERLAPS #-} Pretty AbsEnv where
 -- pPrint e = vcat . map (\(h,adt) -> pPrint h <+> text "->" <+> pPrint (e,adt)) $ M.assocs e
 pPrint e = vspacedP . sortBy (comparing snd) . map (\(h,adt) -> (e,adt)) $ M.assocs e

instance {-# OVERLAPS #-} Pretty (AbsEnv,AbsADT) where
  pPrint (env,adt) = pPrint . stringADT env $ adt

instance Pretty Identifier where pPrint = text . L.toString

instance Pretty a => Pretty (String,ADTRef a) where
   pPrint (_,Var v) = varP v
   pPrint (n,Rec)   = text n
   pPrint (_,Ext r) = pPrint r

instance Pretty a => Pretty (ADTRef a) where
   pPrint (Var v) = varP v
   pPrint Rec     = char '\x21AB'
   pPrint (Ext r) = pPrint r

instance Pretty AbsRef where pPrint (AbsRef sha3) = pPrint sha3

instance Pretty a => Pretty (SHA3_256_6 a) where pPrint (SHA3_256_6 k1 k2 k3 k4 k5 k6) = char 'S' <> prettyWords [k1,k2,k3,k4,k5,k6]

prettyWords = text . concatMap hex

hex = printf "%02x"

unPrettyRef ('S':code) = let [k1,k2,k3,k4,k5,k6] = readHexCode code in SHA3_256_6 k1 k2 k3 k4 k5 k6

readHexCode = readCode []

readCode bs [] = reverse bs
readCode bs s  = let (h,t) = splitAt 2 s
                  in readCode (rdHex h : bs) t

rdHex :: String -> Word8
rdHex s = let [(b,"")] = readP_to_S ((readS_to_P readHex)) s in b

-- instance Pretty a => Pretty (Array a) where pPrint (Array vs) = text "Array" <+> pPrint vs
instance Pretty a => Pretty (Array a) where pPrint (Array vs) = pPrint vs

instance Pretty a => Pretty (NonEmptyList a) where pPrint = pPrint . toList

prettyTuple = parens . fsep . punctuate comma

instance (Pretty a,Pretty l) => Pretty (Label a l) where
   pPrint (Label a Nothing)  = pPrint a
   pPrint (Label _ (Just l)) = pPrint l

-- Instances for standard types
instance Pretty T.Text where pPrint = text . T.unpack

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

pPrint0 l = pPrintPrec l 0

