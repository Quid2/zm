{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Typed.Type.Unit where
import Data.Model

-- |The Unit type
data Unit = Unit deriving (Eq, Ord, Show, Generic, Model)
