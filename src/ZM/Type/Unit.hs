{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ZM.Type.Unit where
import Data.Model

-- |The Unit type
data Unit = Unit deriving (Eq, Ord, Show, Generic, Model)
