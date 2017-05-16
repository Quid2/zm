{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Typed.Type.Bits8 where
import Data.Typed.Type.Bit
import Data.Flat
import Data.Model

data Bits8 =
       Bits8
         { bit0 :: Bit
         , bit1 :: Bit
         , bit2 :: Bit
         , bit3 :: Bit
         , bit4 :: Bit
         , bit5 :: Bit
         , bit6 :: Bit
         , bit7 :: Bit
         }
  deriving (Eq, Ord, Show, Generic, Flat, Model)
