{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module ZM.Type.List
  ( List(..)
  )
where

import           Control.DeepSeq
import           Flat
import           Data.Model

-- |A list
data List a = Nil
             | Cons a (List a)
  deriving (Eq, Ord, Show, NFData, Generic, Functor, Foldable, Traversable, Flat)

instance Model a => Model (List a)
