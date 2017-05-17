{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ZM.Type.Char where
import Data.Model

import ZM.Type.Words

-- |A Unicode Char
data Char = Char Word32 deriving (Eq, Ord, Show, Generic, Model)

