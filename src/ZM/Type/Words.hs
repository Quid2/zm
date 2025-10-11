{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ZM.Type.Words
  ( Sign (..),
    Word7 (..),
    Word (..),
    Word8 (..),
    Word16 (..),
    Word32 (..),
    Word64 (..),
    Int (..),
    Int8 (..),
    Int16 (..),
    Int32 (..),
    Int64 (..),
    ZigZag (..),
    MostSignificantFirst (..),
    LeastSignificantFirst (..),
  )
where

import Data.Model
import qualified Data.Word as H
import Flat
import ZM.Type.Generate
import ZM.Type.NonEmptyList
import Prelude hiding
  ( Int,
    Word,
  )

-- | A 7 bits unsigned integer
--  newtype Word7 = V0 .. V127
newtype Word7 = Word7 H.Word8 deriving (Eq, Ord, Read, Show, Generic)

instance Model Word7 where
  envType = useCT word7CT

-- | An 8 bits unsigned integer
--  newtype Word8 = V0 | V1 .. | V255
newtype Word8 = Word8 H.Word8 deriving (Eq, Ord, Read, Show, Generic, Flat)

instance Model Word8 where
  envType = useCT word8CT

-- |
-- An unsigned integer of arbitrary length encoded as a non empty list of Word7 words with least significant word first and, inside each word, most significant bit first.
--
-- Example:
-- 3450 :: Word
--
-- Binary representation: 0000110101111010
--
-- Split in 7bits groups: 0011010(==26 decimal) 1111010(==122 decimal)
--
-- Build a non-empty list whose elements are the groups in reverse order: Word (Cons V122 (Elem V26))
--
-- So Least Significant Byte first with Most Significant Bit first in every 7 bits group.
--
-- -- BUG: MostSignificantFirst is useless, Word7 already has an order.
newtype Word = Word (LeastSignificantFirst (NonEmptyList (MostSignificantFirst Word7)))
  deriving (Eq, Ord, Show, Generic, Model)

newtype Word16 = Word16 Word
  deriving (Eq, Ord, Show, Generic, Model)

newtype Word32 = Word32 Word
  deriving (Eq, Ord, Show, Generic, Model)

newtype Word64 = Word64 Word
  deriving (Eq, Ord, Show, Generic, Model)

newtype Int = Int (ZigZag Word) deriving (Eq, Ord, Show, Generic, Model)

newtype Int8 = Int8 (ZigZag Word8)
  deriving (Eq, Ord, Show, Generic, Model)

newtype Int16 = Int16 (ZigZag Word16)
  deriving (Eq, Ord, Show, Generic, Model)

newtype Int32 = Int32 (ZigZag Word32)
  deriving (Eq, Ord, Show, Generic, Model)

newtype Int64 = Int64 (ZigZag Word64)
  deriving (Eq, Ord, Show, Generic, Model)

-- | ZigZag encoding, map signed integers to unsigned integers
--  Positive integers are mapped to even unsigned values, negative integers to odd values:
--  0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, 2 -> 4 ...
newtype ZigZag a = ZigZag a
  deriving (Eq, Ord, Show, Generic, Flat)

instance (Model a) => Model (ZigZag a)

newtype LeastSignificantFirst a = LeastSignificantFirst a
  deriving (Eq, Ord, Show, Generic, Flat)

instance (Model a) => Model (LeastSignificantFirst a)

newtype MostSignificantFirst a = MostSignificantFirst a
  deriving (Eq, Ord, Show, Generic, Flat)

instance (Model a) => Model (MostSignificantFirst a)

data Sign = Positive | Negative deriving (Eq, Ord, Show, Generic, Model, Flat)
