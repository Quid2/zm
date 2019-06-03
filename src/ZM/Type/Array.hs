{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric ,DeriveFoldable #-}
module ZM.Type.Array(Array(..)
                    ,Bytes(..)
                    ) where
import           Data.Flat.Filler
import           Data.Model
import           ZM.Type.Generate
import qualified ZM.Type.Words    as Z
import Data.Flat
-- import qualified Data.Word    as H
import           Data.Flat.Encoder
import           Data.Flat.Decoder

{-|An Array.

A sequence of sequences of up to to 255 values.
@
Array a  = A0
         | A1 a (Array a)
         | A2 a a (Array a)
         ...
         | A255 a ... (Array a)
@
-}
data Array a = Array [a] deriving (Eq,Ord,Show,Foldable)
--data Array a = Array (S.Seq (S.Seq a)) -- A sequence of non null sequences
instance Model a => Model (Array a) where envType = useCT arrayCT

instance Flat a => Flat (Array a) where
    --encode (Array ss) = mapM_ enc ss where enc s = encode (fromIntegral (S.length s)::H.Word8) >> foldMap encode s 
    encode (Array l) = encodeArrayWith encode l
    decode = Array <$> decodeArrayWith decode
    size l n = foldr size (n + arrayBits (length l)) l

-- |A byte-aligned byte array
-- To encode and decode efficiently an Array of Bytes, we pre-align it to the nearest byte border.
data Bytes = Bytes (PreAligned (Array Z.Word8)) deriving (Generic,Model)

instance Model Filler
instance Model a => Model (PreAligned a)
