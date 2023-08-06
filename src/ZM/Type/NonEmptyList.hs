{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
module ZM.Type.NonEmptyList
  ( NonEmptyList(..)
  , nonEmptyList
  )
where

import           Control.DeepSeq
import           Data.Model
import           Flat

-- | A list that contains at least one element
data NonEmptyList a = Elem a
                    | Cons a (NonEmptyList a)
  deriving (Eq, Ord, Show, NFData, Generic, Functor, Foldable, Traversable, Flat)

instance Model a => Model (NonEmptyList a)

-- | Convert a list to a `NonEmptyList`, returns an error if the list is empty
nonEmptyList :: [a] -> NonEmptyList a
nonEmptyList []      = error "Cannot convert an empty list to NonEmptyList"
nonEmptyList [h    ] = Elem h
nonEmptyList (h : t) = Cons h (nonEmptyList t)
