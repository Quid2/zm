{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ZM.Type.Bits52 where
import ZM.Type.Bit
import Flat
import Data.Model

data Bits52 =
       Bits52
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
         , bit23 :: Bit
         , bit24 :: Bit
         , bit25 :: Bit
         , bit26 :: Bit
         , bit27 :: Bit
         , bit28 :: Bit
         , bit29 :: Bit
         , bit30 :: Bit
         , bit31 :: Bit
         , bit32 :: Bit
         , bit33 :: Bit
         , bit34 :: Bit
         , bit35 :: Bit
         , bit36 :: Bit
         , bit37 :: Bit
         , bit38 :: Bit
         , bit39 :: Bit
         , bit40 :: Bit
         , bit41 :: Bit
         , bit42 :: Bit
         , bit43 :: Bit
         , bit44 :: Bit
         , bit45 :: Bit
         , bit46 :: Bit
         , bit47 :: Bit
         , bit48 :: Bit
         , bit49 :: Bit
         , bit50 :: Bit
         , bit51 :: Bit
         }
  deriving (Eq, Ord, Show, Generic, Flat, Model)
