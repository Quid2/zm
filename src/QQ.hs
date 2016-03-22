{-# LANGUAGE DeriveGeneric #-}
module QQ(Char,List,Word7,Word8,Word16,Word32,Word64,Int8,Int16,Int32,Int64) where
import Prelude(Eq,Ord,Show)
import Data.Model
--import GHC.Generics
import Data.Typed.Types

import QQ.Word7
import QQ.Word8

instance Model a => Model (NonEmptyList a)

data Word16 = Word16 (NonEmptyList Word7) deriving (Eq, Ord, Show, Generic)
instance Model Word16

data Word32 = Word32 (NonEmptyList Word7) deriving (Eq, Ord, Show, Generic)
instance Model Word32

data Word64 = Word64 (NonEmptyList Word7) deriving (Eq, Ord, Show, Generic)
instance Model Word64

data ZigZag a = ZigZag a deriving (Eq, Ord, Show, Generic)
instance Model a => Model (ZigZag a)

data Int8 = Int8 (ZigZag Word8) deriving (Eq, Ord, Show, Generic)
instance Model Int8

data Int16 = Int16 (ZigZag Word16) deriving (Eq, Ord, Show, Generic)
instance Model Int16

data Int32 = Int32 (ZigZag Word32) deriving (Eq, Ord, Show, Generic)
instance Model Int32

data Int64 = Int64 (ZigZag Word64) deriving (Eq, Ord, Show, Generic)
instance Model Int64

data Char = Chat Word32 deriving (Eq, Ord, Show, Generic)
instance Model Char

data List a = Nil
             | Cons a (List a) deriving (Eq, Ord, Show, Generic)
instance Model a => Model (List a)
