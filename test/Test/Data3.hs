{-# LANGUAGE MultiParamTypeClasses ,DeriveGeneric ,DeriveDataTypeable ,ScopedTypeVariables ,GADTs ,NoMonomorphismRestriction ,DeriveGeneric ,DefaultSignatures ,TemplateHaskell ,TypeFamilies ,FlexibleContexts ,FlexibleInstances ,EmptyDataDecls #-}
module Test.Data3 where

import           Data.Typeable
import           Data.Data
import           GHC.Generics

-- A definition identical to the one in Test.Data, used to test for name clashes.
data List a = C a (List a)
            | N
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic ,Generic1)

