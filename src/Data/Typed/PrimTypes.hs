{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typed.PrimTypes(Char,List,String,Word7,Word8,Word16,Word32,Word64,Int8,Int16,Int32,Int64,Integer,ZigZag,NonEmptyList,Array) where
import           Data.Flat
import           Data.Model
import           Data.Typed.Generate
import           Data.Typed.Types
import           Data.Word           (Word8)
import           Prelude             (Eq, Ord, Show, return)

instance Model a => Model (Array a) where
  envType p = do
    -- envType (Proxy::Proxy a)
    addCT p (return arrayCT)

data String = String (Array Char) deriving (Generic)
instance Model String

instance Model Word8 where envType p = addCT p (return word8CT)

data Word7 = Word7 Word8 deriving (Eq, Ord, Show, Generic)
instance Model Word7 where envType p = addCT p (return word7CT)

instance Model a => Model (NonEmptyList a)

data Word16 = Word16 (NonEmptyList Word7) deriving (Eq, Ord, Show, Generic,Model)

data Word32 = Word32 (NonEmptyList Word7) deriving (Eq, Ord, Show, Generic, Model)

data Word64 = Word64 (NonEmptyList Word7) deriving (Eq, Ord, Show, Generic, Model)

-- Better
-- data Word64 = Word64 Word deriving (Eq, Ord, Show, Generic, Model)

data Word = Word (LeastSignificantFirst (NonEmptyList Word7)) deriving (Eq, Ord, Show, Generic, Model)

data LeastSignificantFirst a = LeastSignificantFirst a deriving (Eq, Ord, Show, Generic)
instance Model a => Model (LeastSignificantFirst a)

-- Indicate ZigZag encoding
data ZigZag a = ZigZag a deriving (Eq, Ord, Show, Generic)
instance Model a => Model (ZigZag a)

data Int8 = Int8 (ZigZag Word8) deriving (Eq, Ord, Show, Generic, Model)

data Int16 = Int16 (ZigZag Word16) deriving (Eq, Ord, Show, Generic, Model)

data Int32 = Int32 (ZigZag Word32) deriving (Eq, Ord, Show, Generic, Model)

data Int64 = Int64 (ZigZag Word64) deriving (Eq, Ord, Show, Generic, Model)

data Integer = Integer (ZigZag (NonEmptyList Word7)) deriving (Eq, Ord, Show, Generic, Model)

data Int = Int (ZigZag Word) deriving (Eq, Ord, Show, Generic, Model)

data Char = Char Word32 deriving (Eq, Ord, Show, Generic, Model)

data List a = Nil
             | Cons a (List a) deriving (Eq, Ord, Show, Generic)
instance Model a => Model (List a)


