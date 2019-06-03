{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.ByType.K87f090a54ea3 (ByType(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model

data ByType a =   ByType
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( ByType a )
