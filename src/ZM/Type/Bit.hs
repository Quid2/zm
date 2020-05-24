{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ZM.Type.Bit where
import Flat
import Data.Model

-- | A Bit
data Bit = V0 | V1 deriving (Eq,Ord,Show,Generic,Flat,Model)
