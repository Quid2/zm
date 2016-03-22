{-# LANGUAGE DeriveGeneric #-}
module QQ(Char,List,Word7,Word8,Word16,Word32) where
import Prelude(Eq,Ord,Show)
import Data.Model
import GHC.Generics
import Data.Typed.Types

import QQ.Word7
import QQ.Word8

instance Model a => Model (NonEmptyList a)

data Word16 = Word16 (NonEmptyList Word7) deriving (Eq, Ord, Show, Generic)
instance Model Word16

data Word32 = Word32 (NonEmptyList Word7) deriving (Eq, Ord, Show, Generic)
instance Model Word32

data Char = Chat Word32 deriving (Eq, Ord, Show, Generic)
instance Model Char

data List a = Nil
             | Cons a (List a) deriving (Eq, Ord, Show, Generic)
instance Model a => Model (List a)
