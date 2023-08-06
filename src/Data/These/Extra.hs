module Data.These.Extra
  ( These,
  -- , equivalent
    aThese
  , both
  , hasThis
  , hasThat
  , fromThis
  , fromThat
  ) where

import           Data.Maybe (fromJust, isJust)
import           Data.These

-- |A data type that represents either a value of type `this`, or a value of type `that`, or both.
-- data These this that
--   = This this
--   | That that
--   | These this that
--   deriving (Eq,Ord, Read, Show)

{-
Does not satisfy extensionality:
f (This _) = 0
f (That _) = 1
f (These _ _) = 2

prop> \(x::These Bool Bool) -> x == x

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

-- |Constructor function, from Maybes
aThese :: Maybe a -> Maybe b -> These a b
aThese Nothing Nothing =
  error "aThese:you must provide either this or that or both"
aThese Nothing (Just b) = That b
aThese (Just a) Nothing = This a
aThese (Just a) (Just b) = These a b


-- ??? use a different kind of eq?
-- instance (Eq a, Eq b) => Eq (These a b) where
--   t1 == t2 =
-- equivalent :: (Eq a1, Eq a) => These a1 a -> These a1 a -> Bool
-- t1 `equivalent` t2 =
--     (maybeThis t1 `meq` maybeThis t2) && (maybeThat t1 `meq` maybeThat t2)
--     where
--       meq Nothing _         = True
--       meq _ Nothing         = True
--       meq (Just a) (Just b) = a == b

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

hasThat :: These a1 a -> Bool
hasThat = isJust . maybeThat

fromThis :: These c b -> c
fromThis = fromJust . maybeThis

fromThat :: These a1 c -> c
fromThat = fromJust . maybeThat

maybeThis :: These a b -> Maybe a
maybeThis (This a)    = Just a
maybeThis (These a _) = Just a
maybeThis _           = Nothing

maybeThat :: These a1 a2 -> Maybe a2
maybeThat (That b)    = Just b
maybeThat (These _ b) = Just b
maybeThat _           = Nothing


{-
Eliminator function

>>> both ((:[]) . not) ((:[]) . (>0)) $ These False (3::Int)
[True,True]

>>> both ((:[]) . not) (:[]) $ This False
[True]
-}
both :: Semigroup a => (t1 -> a) -> (t2 -> a) -> These t1 t2 -> a
both f g =  these f g (\a b -> f a <> g b)
