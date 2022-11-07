module Data.These
  ( These(..)
  , both
  , hasThis
  , hasThat
  , this
  , that
  , these
  ) where

import Data.Maybe ( fromJust, isJust )

-- |A data type that represents either a value of type `a`, or a value of type `b`, or both.
data These a b
  = This a
  | That b
  | These a b
  deriving (Ord, Read, Show)

{-
>>> This 'a' == These 'a' False
True

>>> This 'b' == These 'a' False
False

>>> That False == These 'a' False
True

>>> These 'a' False == That False
True

>>> These 'a' False == That False
True

>>> This 'a'== That 'a'
True
-}
-- ??? use a different kind of eq?
instance (Eq a, Eq b) => Eq (These a b) where
  t1 == t2 =
    (maybeThis t1 `meq` maybeThis t2) && (maybeThat t1 `meq` maybeThat t2)
    where
      meq Nothing _         = True
      meq _ Nothing         = True
      meq (Just a) (Just b) = a == b

-- This a1 == This a2 = a1 == a2
-- This a1 == These a2 _ = a1 == a2
-- That b1 == That b2 = b1 == b2
-- That b1 == These _ b2 = b1 == b2
-- These a1 b1 == These a2 b2 = a1 == a2 && b1 == b2
-- These a1 _ == This a2 = a1 == a2
-- These _ b1 == That b2 = b1 == b2
-- _ == _ = False
hasThis :: These a b -> Bool
hasThis = isJust . maybeThis

this :: These c b -> c
this = fromJust . maybeThis

maybeThis :: These a b -> Maybe a
maybeThis (This a)    = Just a
maybeThis (These a _) = Just a
maybeThis _           = Nothing

hasThat :: These a1 a -> Bool
hasThat = isJust . maybeThat

that :: These a1 c -> c
that = fromJust . maybeThat

maybeThat :: These a1 a2 -> Maybe a2
maybeThat (That b)    = Just b
maybeThat (These _ b) = Just b
maybeThat _           = Nothing

-- |Constructor function, from Maybes
these :: Maybe a -> Maybe b -> These a b
these Nothing Nothing =
  error "these:you must provide either this or that or both"
these Nothing (Just b) = That b
these (Just a) Nothing = This a
these (Just a) (Just b) = These a b

{-
Eliminator function

>>> both ((:[]) . not) ((:[]) . (>0)) $ These False (3::Int)
[True,True]

>>> both ((:[]) . not) (:[]) $ This False
[True]
-}
both :: Semigroup a => (t1 -> a) -> (t2 -> a) -> These t1 t2 -> a
both f _ (This a)    = f a
both _ g (That a)    = g a
both f g (These a b) = f a <> g b
