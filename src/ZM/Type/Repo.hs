{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ZM.Type.Repo
    --RepoProtocol(..)
                    where

import           Flat
import           Data.Model
import           Prelude                 hiding ( String )

-- import           ZM
-- import           ZM.Type.String
-- {-|
-- A (simplistic) protocol to permanently store and retrieve ADT definitions.
-- -}
-- data RepoProtocol = Record AbsADT                      -- ^Permanently record an absolute type
--                   | Solve AbsRef                       -- ^Retrieve the absolute type
--                   | Solved AbsRef AbsADT               -- ^Return the absolute type identified by an absolute reference
--                   | AskDataTypes                       -- ^Request the list of all known data types
--                   | KnownDataTypes [(AbsRef, AbsADT)]  -- ^Return the list of all known data types
--                   | AskDataTypesRefs                       -- ^Request the list of all known data types references (absolute references and names)
--                   | KnownDataTypesRefs [(AbsRef, String)]  -- ^Return the list of all known data types
--   deriving (Eq, Ord, Show, Generic, Flat, Model)
-- Ask for all known values of type a
data AllKnown a =
  AllKnown
  deriving (Eq, Ord, Show, Generic, Flat)

instance Model a => Model (AllKnown a)

-- Record a value of type a
data Record a =
  Record a
  deriving (Eq, Ord, Show, Generic, Flat)

instance (Model a) => Model (Record a)

-- Solve a reference of type to the corresponding value of type to if it exists.
data Solve ref to =
  Solve ref
  deriving (Eq, Ord, Show, Generic, Flat)

instance (Model a, Model b) => Model (Solve a b)
