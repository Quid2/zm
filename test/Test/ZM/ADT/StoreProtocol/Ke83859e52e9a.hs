{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.StoreProtocol.Ke83859e52e9a (StoreProtocol(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.SHAKE128_48.K9f214799149b

data StoreProtocol a =   Save a
                       | Solve (Test.ZM.ADT.SHAKE128_48.K9f214799149b.SHAKE128_48 a)
                       | Solved (Test.ZM.ADT.SHAKE128_48.K9f214799149b.SHAKE128_48 a) a
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( StoreProtocol a )
