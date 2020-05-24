{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.PreAligned.Kb2f28cf37d12 (PreAligned(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Filler.Kae1dfeece189

data PreAligned a =   PreAligned {preFiller :: Test.ZM.ADT.Filler.Kae1dfeece189.Filler,
                                  preValue :: a}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( PreAligned a )
