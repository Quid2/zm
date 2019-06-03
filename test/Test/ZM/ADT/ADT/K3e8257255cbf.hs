{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.ADT.K3e8257255cbf (ADT(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Word8.Kb1f46a49c8f8
import qualified Test.ZM.ADT.Maybe.Kda6836778fd4
import qualified Test.ZM.ADT.ConTree.K86653e040025

data ADT a b c =   ADT {declName :: a,
                        declNumParameters :: Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8,
                        declCons :: Test.ZM.ADT.Maybe.Kda6836778fd4.Maybe (Test.ZM.ADT.ConTree.K86653e040025.ConTree b
                                                                                                                     c)}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance ( Data.Model.Model a,Data.Model.Model b,Data.Model.Model c ) => Data.Model.Model ( ADT a b c )
