{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Typed.Type.Map(Map) where
import           Data.Model
import           Data.Typed.Type.List
import           Data.Typed.Type.Tuples

-- |A Map is represented as a list of key and value couples
data Map a b = Map (List (Tuple2 a b)) deriving Generic

instance (Model a, Model b) => Model (Map a b)
