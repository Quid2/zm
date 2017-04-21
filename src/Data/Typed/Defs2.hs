{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Typed.Defs2 (IEEE_754_binary64(..)) where

import           Data.Flat
import           Data.Model
import           Data.Typed.Defs hiding (IEEE_754_binary32)
import Data.Typed.Class
import Data.Typed.Types

data IEEE_754_binary64 =
       IEEE_754_binary64
         { sign :: Sign
         , exponent :: MostSignificantFirst Bits11
         , fraction :: MostSignificantFirst Bits52
         }
  deriving (Eq, Ord, Show, Generic, Model, Flat)

