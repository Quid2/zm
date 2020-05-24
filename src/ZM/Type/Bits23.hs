{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ZM.Type.Bits23 where
import           ZM.Type.Bit
import           Flat
import           Data.Model

data Bits23 =
       Bits23
         { bit0 :: Bit
         , bit1 :: Bit
         , bit2 :: Bit
         , bit3 :: Bit
         , bit4 :: Bit
         , bit5 :: Bit
         , bit6 :: Bit
         , bit7 :: Bit
         , bit8 :: Bit
         , bit9 :: Bit
         , bit10 :: Bit
         , bit11 :: Bit
         , bit12 :: Bit
         , bit13 :: Bit
         , bit14 :: Bit
         , bit15 :: Bit
         , bit16 :: Bit
         , bit17 :: Bit
         , bit18 :: Bit
         , bit19 :: Bit
         , bit20 :: Bit
         , bit21 :: Bit
         , bit22 :: Bit
         }
  deriving (Eq, Ord, Show, Generic, Model, Flat)
