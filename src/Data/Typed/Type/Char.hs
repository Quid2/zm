{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Typed.Type.Char where
import Data.Model

import Data.Typed.Type.Words

-- |A Unicode Char
data Char = Char Word32 deriving (Eq, Ord, Show, Generic, Model)

