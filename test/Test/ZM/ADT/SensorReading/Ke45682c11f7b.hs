{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.SensorReading.Ke45682c11f7b (SensorReading(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model

data SensorReading a b =   SensorReading {reading :: a,
                                          location :: b}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance ( Data.Model.Model a,Data.Model.Model b ) => Data.Model.Model ( SensorReading a b )
