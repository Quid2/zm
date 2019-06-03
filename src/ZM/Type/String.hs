{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
module ZM.Type.String (String(..)) where

import Prelude hiding (String)
import           Data.Model
import Data.Flat
import ZM.Model ()

data String = String [Char] deriving (Eq, Ord, Show, Generic, Flat, Model)  