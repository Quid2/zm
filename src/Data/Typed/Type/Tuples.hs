{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
module Data.Typed.Type.Tuples (
    Tuple2(..),
    Tuple3(..),
    Tuple4(..),
    Tuple5(..),
    Tuple6(..),
    Tuple7(..),
    Tuple8(..),
    Tuple9(..)
    ) where

import           Data.Model

data Tuple2 a b = Tuple2 a b deriving (Eq, Ord, Show, Generic)
instance (Model a,Model b) => Model (Tuple2 a b)

data Tuple3 a b c = Tuple3 a b c deriving (Eq, Ord, Show, Generic)
instance (Model a,Model b,Model c) => Model (Tuple3 a b c)

data Tuple4 a b c d = Tuple4 a b c d deriving (Eq, Ord, Show, Generic)
instance (Model a,Model b,Model c,Model d) => Model (Tuple4 a b c d)

data Tuple5 a1 a2 a3 a4 a5 = Tuple5 a1 a2 a3 a4 a5 deriving (Eq, Ord, Show, Generic)
instance (Model a1,Model a2,Model a3,Model a4,Model a5) => Model (Tuple5 a1 a2 a3 a4 a5)

data Tuple6 a1 a2 a3 a4 a5 a6 = Tuple6 a1 a2 a3 a4 a5 a6 deriving (Eq, Ord, Show, Generic)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6) => Model (Tuple6 a1 a2 a3 a4 a5 a6)

data Tuple7 a1 a2 a3 a4 a5 a6 a7= Tuple7 a1 a2 a3 a4 a5 a6 a7 deriving (Eq, Ord, Show, Generic)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7) => Model (Tuple7 a1 a2 a3 a4 a5 a6 a7)

data Tuple8 a1 a2 a3 a4 a5 a6 a7 a8 = Tuple8 a1 a2 a3 a4 a5 a6 a7 a8 deriving (Eq, Ord, Show, Generic)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7,Model a8) => Model (Tuple8 a1 a2 a3 a4 a5 a6 a7 a8)

data Tuple9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = Tuple9 a1 a2 a3 a4 a5 a6 a7 a8 a9 deriving (Eq, Ord, Show, Generic)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7,Model a8,Model a9) => Model (Tuple9 a1 a2 a3 a4 a5 a6 a7 a8 a9)
