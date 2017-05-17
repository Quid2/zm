{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ZM.Type.Map(Map) where
import           Data.Model
import           ZM.Type.List
import           ZM.Type.Tuples

-- |A Map is represented as a list of key and value couples
data Map a b = Map (List (Tuple2 a b)) deriving Generic

instance (Model a, Model b) => Model (Map a b)
