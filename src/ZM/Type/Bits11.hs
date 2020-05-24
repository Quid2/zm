{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ZM.Type.Bits11 where
import           ZM.Type.Bit
import           Flat
import           Data.Model

data Bits11 =
       Bits11
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
         }
  deriving (Eq, Ord, Show, Generic, Flat, Model)
