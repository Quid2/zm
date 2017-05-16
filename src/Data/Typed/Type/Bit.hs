{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Typed.Type.Bit where
import Data.Flat
import Data.Model

-- | A Bit
data Bit = V0 | V1 deriving (Eq,Ord,Show,Generic,Flat,Model)
