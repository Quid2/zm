module ZM.Pretty.Base (prettyWords,hex) where

import           Data.Word
import           Text.PrettyPrint.HughesPJClass
import           Text.Printf
import           ZM.Types

instance Pretty AbsRef where pPrint (AbsRef sha3) = pPrint sha3

instance Pretty (SHA3_256_6 a) where pPrint (SHA3_256_6 k1 k2 k3 k4 k5 k6) = char 'S' <> prettyWords [k1,k2,k3,k4,k5,k6]

instance Pretty (SHAKE128_48 a) where pPrint (SHAKE128_48 k1 k2 k3 k4 k5 k6) = char 'K' <> prettyWords [k1,k2,k3,k4,k5,k6]

-- |Display a list of Words in hexadecimal format
--
-- >>> prettyWords [11,22,33::Word8]
-- 0b1621
prettyWords :: [Word8] -> Doc
prettyWords = text . concatMap hex

-- |Display a Word in hexadecimal format
hex :: Word8 -> String
hex = printf "%02x"

