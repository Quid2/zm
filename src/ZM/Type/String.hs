{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ZM.Type.String
  ( String(..)
  )
where

import           Control.DeepSeq
import           Flat
import           Data.Model
import           Prelude                 hiding ( String )
import           ZM.Model                       ( )

data String =
  String [Char]
  deriving (Eq, Ord, Show, Generic, Flat, Model, NFData)
