{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ZM.Type.Float64(IEEE_754_binary64(..)) where

import           Data.Model
import           ZM.Type.Bits11
import           ZM.Type.Bits52
import           ZM.Type.Words

-- |An IEEE-754 Big Endian 64 bits Float
data IEEE_754_binary64 =
       IEEE_754_binary64
         { sign :: Sign
         , exponent :: MostSignificantFirst Bits11
         , fraction :: MostSignificantFirst Bits52
         }
  deriving (Eq, Ord, Show, Generic, Model)

