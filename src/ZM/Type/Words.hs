{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ZM.Type.Words
  ( Sign(..)
  , Word7(..)
  , Word(..)
  , Word8(..)
  , Word16(..)
  , Word32(..)
  , Word64(..)
  , Int(..)
  , Int8(..)
  , Int16(..)
  , Int32(..)
  , Int64(..)
  , ZigZag(..)
  , MostSignificantFirst(..)
  , LeastSignificantFirst(..)
  )
where

import           Prelude                 hiding ( Word
                                                , Int
                                                )
import           Flat
import           Data.Model
import           ZM.Type.NonEmptyList
import           ZM.Type.Generate
import qualified Data.Word                     as H


-- |A 7 bits unsigned integer
-- data Word7 = V0 .. V127
data Word7 = Word7 H.Word8 deriving (Eq, Ord, Read, Show, Generic)
instance Model Word7 where
  envType = useCT word7CT

-- |An 8 bits unsigned integer
-- data Word8 = V0 | V1 .. | V255
data Word8 = Word8 H.Word8 deriving (Eq, Ord, Read, Show, Generic)
instance Model Word8 where
  envType = useCT word8CT

{- |
An unsigned integer of arbitrary length encoded as a non empty list of Word7 words with least significant word first and, inside each word, most significant bit first.

Example:
3450 :: Word

Binary representation: 0000110101111010

Split in 7bits groups: 0011010(==26 decimal) 1111010(==122 decimal)

Build a non-empty list whose elements are the groups in reverse order: Word (Cons V122 (Elem V26))

So Least Significant Byte first with Most Significant Bit first in every 7 bits group.

-- BUG: MostSignificantFirst is useless, Word7 already has an order.
-}
data Word = Word (LeastSignificantFirst (NonEmptyList (MostSignificantFirst Word7)))
  deriving (Eq, Ord, Show, Generic, Model)

data Word16 = Word16 Word
  deriving (Eq, Ord, Show, Generic, Model)

data Word32 = Word32 Word
  deriving (Eq, Ord, Show, Generic, Model)

data Word64 = Word64 Word
  deriving (Eq, Ord, Show, Generic, Model)



data Int = Int (ZigZag Word) deriving (Eq, Ord, Show, Generic, Model)

data Int8 = Int8 (ZigZag Word8)
  deriving (Eq, Ord, Show, Generic, Model)

data Int16 = Int16 (ZigZag Word16)
  deriving (Eq, Ord, Show, Generic, Model)

data Int32 = Int32 (ZigZag Word32)
  deriving (Eq, Ord, Show, Generic, Model)

data Int64 = Int64 (ZigZag Word64)
  deriving (Eq, Ord, Show, Generic, Model)

-- |ZigZag encoding, map signed integers to unsigned integers
-- Positive integers are mapped to even unsigned values, negative integers to odd values:
-- 0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, 2 -> 4 ...
data ZigZag a = ZigZag a
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a => Model (ZigZag a)

data LeastSignificantFirst a = LeastSignificantFirst a
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a => Model (LeastSignificantFirst a)

data MostSignificantFirst a = MostSignificantFirst a
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a => Model (MostSignificantFirst a)

data Sign = Positive | Negative deriving (Eq, Ord, Show, Generic, Model, Flat)
