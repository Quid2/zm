{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.ConTree.K86653e040025 (ConTree(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Either.K6260e465ae74
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.Type.K7028aa556ebc
import qualified Test.ZM.ADT.Tuple2.Ka5583bf3ad34

data ConTree a b =   Con {constrName :: a,
                          constrFields :: Test.ZM.ADT.Either.K6260e465ae74.Either (Test.ZM.ADT.List.Kb8cd13187198.List (Test.ZM.ADT.Type.K7028aa556ebc.Type b))
                                                                                  (Test.ZM.ADT.List.Kb8cd13187198.List (Test.ZM.ADT.Tuple2.Ka5583bf3ad34.Tuple2 a
                                                                                                                                                                (Test.ZM.ADT.Type.K7028aa556ebc.Type b)))}
                   | ConTree (Test.ZM.ADT.ConTree.K86653e040025.ConTree a b)
                             (Test.ZM.ADT.ConTree.K86653e040025.ConTree a b)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance ( Data.Model.Model a,Data.Model.Model b ) => Data.Model.Model ( ConTree a b )
