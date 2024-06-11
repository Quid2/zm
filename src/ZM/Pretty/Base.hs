module ZM.Pretty.Base (
  prettyWords,
  hex,
) where

import Data.Word
import Text.PrettyPrint.HughesPJClass
import qualified Text.PrettyPrint.HughesPJClass as P
import Text.Printf
import ZM.Types
import Prelude

instance (Pretty t) => Pretty (ZMError t) where
  pPrint (UnknownType t) = text "Reference to unknown type: " P.<> pPrint t
  pPrint (WrongKind t expPars actPars) =
    hsep
      [ text "Incorrect application of"
      , pPrint t P.<> text ", should have"
      , text $ show expPars
      , text "parameters but has"
      , text $ show actPars
      ]
  pPrint (MutuallyRecursive ts) =
    text "Found mutually recursive types: " P.<> pPrint ts

instance Pretty AbsRef where
  pPrint (AbsRef sha3) = pPrint sha3

instance Pretty (SHA3_256_6 a) where
  pPrint (SHA3_256_6 k1 k2 k3 k4 k5 k6) =
    char 'S' P.<> prettyWords [k1, k2, k3, k4, k5, k6]

instance Pretty (SHAKE128_48 a) where
  pPrint (SHAKE128_48 k1 k2 k3 k4 k5 k6) =
    char 'K' P.<> prettyWords [k1, k2, k3, k4, k5, k6]

{- | Display a list of Words in hexadecimal format

>>> prettyWords [22,33,44::Word8]
16212c
-}
prettyWords :: [Word8] -> Doc
prettyWords = text . concatMap hex

-- | Display a Word in hexadecimal format
hex :: Word8 -> String
hex = printf "%02x"
