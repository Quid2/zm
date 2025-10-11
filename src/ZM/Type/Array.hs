{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}

module ZM.Type.Array
  ( Array (..),
    Bytes (..),
  )
where

import Control.DeepSeq
import Data.Model
import qualified Data.Word as H
import Flat
import Flat.Decoder
import Flat.Encoder
import ZM.Type.Generate
import ZM.Type.Prims ()
import qualified ZM.Type.Words as Z

-- | An Array.
--
-- A sequence of sequences of up to to 255 values.
-- @
-- Array a  = A0
--          | A1 a (Array a)
--          | A2 a a (Array a)
--          ...
--          | A255 a ... (Array a)
-- @
newtype Array a
  = Array [a]
  deriving (Eq, Ord, Show, Foldable, Generic, NFData)

-- data Array a = Array (S.Seq (S.Seq a)) -- A sequence of non null sequences
instance (Model a) => Model (Array a) where
  envType = useCT arrayCT

instance (Flat a) => Flat (Array a) where
  -- encode (Array ss) = mapM_ enc ss where enc s = encode (fromIntegral (S.length s)::H.Word8) >> foldMap encode s

  encode (Array l) = encodeArrayWith encode l
  decode = Array <$> decodeArrayWith decode
  size l n = foldr size (n + arrayBits (length l)) l

-- | A byte-aligned byte array
--  To encode and decode efficiently an Array of Bytes, we pre-align it to the nearest byte border.
newtype Bytes
  = Bytes (PreAligned (Array H.Word8))
  deriving (Eq, Ord, Show, Generic, NFData, Flat, Model)

-- newtype AlignedBytes v
--   = AlignedBytes (PreAligned (Array v))
--   deriving (Eq, Show, Generic, Model, Flat, NFData)

-- instance Read Bytes where
--   readPrec = Bytes . preAligned . Array . map Z.Word8 <$> readPrec
instance Model Filler

instance (Model a) => Model (PreAligned a)
