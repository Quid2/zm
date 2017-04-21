{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Typed.Defs (
    IEEE_754_binary32(..),
    Sign(..),
    Bit(..),
    -- Bits2(..),
    -- Bits3(..),
    -- Bits4(..),
    -- Bits5(..),
    -- Bits6(..),
    -- Bits7(..),
    -- Bits8(..),
    Word3(..),
    Word4(..),
    Word7(..),
    Bits11(..),
    Bits23(..),
    Bits52(..),
    ZigZag(..),
    MostSignificantFirst(..),
    LeastSignificantFirst(..),
    ) where

import           Data.Flat
import           Data.Model
import           Data.Typed.Generate
import           Data.Word

data IEEE_754_binary32_LE =
       IEEE_754_binary32_LE
         { fractionLE :: LeastSignificantFirst Bits23
         , exponentLE :: LeastSignificantFirst Bits8
         , signLE     :: Sign
         }

-- or data IEEE_754_binary32_LE = IEEE_754_binary32 Word64

-- Big Endian Double
data IEEE_754_binary32 =
       IEEE_754_binary32
         { sign     :: Sign
         , exponent :: MostSignificantFirst Bits8
         , fraction :: MostSignificantFirst Bits23
         }
  deriving (Eq, Ord, Show, Generic, Model)

data Sign = Positive | Negative deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bit = V0 | V1 deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bits2 = Bits2 Bit Bit deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bits3 = Bits3 Bit Bit Bit deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bits4 = Bits4 Bit Bit Bit Bit deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bits5 = Bits5 Bit Bit Bit Bit Bit deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bits6 = Bits6 Bit Bit Bit Bit Bit Bit deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bits7 = Bits7 Bit Bit Bit Bit Bit Bit Bit deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bits8 = Bits8 Bit Bit Bit Bit Bit Bit Bit Bit deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bits11 = Bits11 Bits3 Bits8 deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bits23 = Bits23 Bits7 Bits8 Bits8 deriving (Eq,Ord,Show,Generic,Flat,Model)

data Bits52 = Bits52 Bits4 Bits8 Bits8 Bits8 Bits8 Bits8 Bits8 deriving (Eq,Ord,Show,Generic,Flat,Model)

data Word11 = Word11 Word3 Word8 deriving (Eq,Ord,Show,Generic,Flat,Model)

data Word23 = Word23 Word7 Word8 Word8 deriving (Eq,Ord,Show,Generic,Flat,Model)

data Word52 = Word52 Word4 Word8 Word8 Word8 Word8 Word8 Word8 deriving (Eq,Ord,Show,Generic,Flat,Model)

data Word4 = Word4 Bit Bit Bit Bit deriving (Eq,Ord,Show,Generic,Flat,Model)

data Word3 = Word3 Bit Bit Bit deriving (Eq,Ord,Show,Generic,Flat,Model)

-- |ZigZag encoding, map signed integers to unsigned integers
-- Positive integers are mapped to even unsigned values, negative integers to odd values:
-- 0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, 2 -> 4 ...
data ZigZag a = ZigZag a
  deriving (Eq, Ord, Show, Generic, Flat)

data LeastSignificantFirst a = LeastSignificantFirst a
  deriving (Eq, Ord, Show, Generic, Flat)

data MostSignificantFirst a = MostSignificantFirst a
  deriving (Eq, Ord, Show, Generic, Flat)

instance Model a => Model (LeastSignificantFirst a)
instance Model a => Model (MostSignificantFirst a)
instance Model a => Model (ZigZag a)

-- |A 7 bits unsigned integer
data Word7 = Word7 Word8 deriving (Eq, Ord, Show, Generic, Flat)

instance Model Word8 where envType = useCT word8CT

instance Model Word7 where envType = useCT word7CT

data SomeStuff

