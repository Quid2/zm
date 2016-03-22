module Data.Typed.Pretty where
import           Text.PrettyPrint.HughesPJClass
import Data.Word
import Data.Typed.Types
import Data.Foldable (toList)
import Text.Printf

instance Pretty (Ref a) where
  pPrint (Verbatim bl) = char 'V' <> prettyNE bl -- pPrint bl
  pPrint (Shake128 bl) = char 'H' <> prettyNE bl -- pPrint bl

instance Pretty a => Pretty (NonEmptyList a) where pPrint = pPrint . toList

instance Pretty Word8 where pPrint = text . hex

prettyNE :: NonEmptyList Word8 -> Doc
prettyNE = cat . map pPrint . toList

hex = printf "%02x"
