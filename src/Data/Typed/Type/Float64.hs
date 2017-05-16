{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Typed.Type.Float64(IEEE_754_binary64(..)) where

import           Data.Model
import           Data.Typed.Type.Bits11
import           Data.Typed.Type.Bits52
import           Data.Typed.Type.Words

-- |An IEEE-754 Big Endian 64 bits Float
data IEEE_754_binary64 =
       IEEE_754_binary64
         { sign :: Sign
         , exponent :: MostSignificantFirst Bits11
         , fraction :: MostSignificantFirst Bits52
         }
  deriving (Eq, Ord, Show, Generic, Model)

