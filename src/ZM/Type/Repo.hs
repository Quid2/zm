{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
module ZM.Type.Repo (RepoProtocol(..)) where

import Prelude hiding (String)
import           Data.Model
import Data.Flat
import ZM.Type.String
import ZM

{-|
A (simplistic) protocol to permanently store and retrieve ADT definitions.
-}
data RepoProtocol = Record AbsADT                      -- ^Permanently record an absolute type
                  | Solve AbsRef                       -- ^Retrieve the absolute type
                  | Solved AbsRef AbsADT               -- ^Return the absolute type identified by an absolute reference
                  | AskDataTypes                       -- ^Request the list of all known data types
                  | KnownDataTypes [(AbsRef, AbsADT)]  -- ^Return the list of all known data types
                  | AskDataTypesRefs                       -- ^Request the list of all known data types references (absolute references and names)
                  | KnownDataTypesRefs [(AbsRef, String)]  -- ^Return the list of all known data types
  deriving (Eq, Ord, Show, Generic, Flat, Model)
