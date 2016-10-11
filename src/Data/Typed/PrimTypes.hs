{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable ,CPP  #-}
module Data.Typed.PrimTypes(
  Word7,ZigZag,Array,List(..)
  -- ,NonEmptyList,nonEmptyList
  --,Char,String,,Word8,Word16,Word32,Word64,Int8,Int16,Int32,Int64,Integer
  ) where

import           Data.Flat
import           Data.Model
import           Data.Typed.Generate
import Data.Word(Word8)
import           qualified Data.Word  as H
import           qualified Data.Int  as H
import qualified Prelude as H
import           Prelude             (Eq, Ord, Show, return,error,Foldable,Functor,Traversable,Read)
import           Control.DeepSeq
import Data.Typed.Types(Word7,NonEmptyList)

#include "MachDeps.h"

data Word16 = Word16 Word deriving (Eq, Ord, Show, Generic,Model)

data Word32 = Word32 Word deriving (Eq, Ord, Show, Generic, Model)

data Word64 = Word64 Word deriving (Eq, Ord, Show, Generic, Model)

-- An unsigned integer coded as a sequence of Word7 words, with least significant word and inside each word, most significant bit first
data Word = Word (LeastSignificantFirst (NonEmptyList (MostSignificantFirst Word7))) deriving (Eq, Ord, Show, Generic,Model)

data LeastSignificantFirst a = LeastSignificantFirst a deriving (Eq, Ord, Show, Generic)

data MostSignificantFirst a = MostSignificantFirst a deriving (Eq, Ord, Show, Generic)

-- Indicate ZigZag encoding
data ZigZag a = ZigZag a deriving (Eq, Ord, Show, Generic)

data Int8 = Int8 (ZigZag Word8) deriving (Eq, Ord, Show, Generic, Model)

data Int16 = Int16 (ZigZag Word16) deriving (Eq, Ord, Show, Generic, Model)

data Int32 = Int32 (ZigZag Word32) deriving (Eq, Ord, Show, Generic, Model)

data Int64 = Int64 (ZigZag Word64) deriving (Eq, Ord, Show, Generic, Model)

-- data Integer = Integer (ZigZag (NonEmptyList Word7)) deriving (Eq, Ord, Show, Generic, Model)

data Int = Int (ZigZag Word) deriving (Eq, Ord, Show, Generic, Model)

data String = String (Array Char) deriving (Generic)

data Char = Char Word32 deriving (Eq, Ord, Show, Generic, Model)

data List a = Nil
             | Cons a (List a) deriving (Eq, Ord, Show, Generic)

#if WORD_SIZE_IN_BITS == 64
instance Model H.Word where envType _ = envType (Proxy::Proxy Word64)
instance Model H.Int where envType _ = envType (Proxy::Proxy Int64)
#elif WORD_SIZE_IN_BITS == 32
instance Model H.Word where envType _ = envType (Proxy::Proxy Word32)
instance Model H.Int where envType _ = envType (Proxy::Proxy Int32)
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif

instance Model a => Model (Array a) where
  envType p = do
    -- envType (Proxy::Proxy a)
    addCT p (return arrayCT)

instance Model a => Model [a] where envType _ = envType (Proxy::Proxy (List a))

instance Model a => Model (NonEmptyList a)
instance Model a => Model (LeastSignificantFirst a)
instance Model a => Model (MostSignificantFirst a)
instance Model a => Model (ZigZag a)
instance Model a => Model (List a)

instance Model Word7 where envType p = addCT p (return word7CT)

instance Model H.Word8 where envType p = addCT p (return word8CT)

instance Model H.Word16 where envType _ = envType (Proxy::Proxy Word16)
instance Model H.Word32 where envType _ = envType (Proxy::Proxy Word32)
instance Model H.Word64 where envType _ = envType (Proxy::Proxy Word64)
instance Model H.Int8 where envType _ = envType (Proxy::Proxy Int8)
instance Model H.Int16 where envType _ = envType (Proxy::Proxy Int16)
instance Model H.Int32 where envType _ = envType (Proxy::Proxy Int32)
instance Model H.Int64 where envType _ = envType (Proxy::Proxy Int64)
--instance Model H.Integer where envType _ = envType (Proxy::Proxy Integer)
instance Model H.Integer where envType _ = envType (Proxy::Proxy Int)

instance Model String
instance Model H.Char where envType _ = envType (Proxy::Proxy Char)

