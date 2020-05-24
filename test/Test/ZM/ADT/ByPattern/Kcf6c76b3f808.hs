{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.ByPattern.Kcf6c76b3f808 (ByPattern(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.Match.Kc23b20389114
import qualified Test.ZM.ADT.Bit.K65149ce3b366

newtype ByPattern a =   ByPattern (Test.ZM.ADT.List.Kb8cd13187198.List (Test.ZM.ADT.Match.Kc23b20389114.Match (Test.ZM.ADT.List.Kb8cd13187198.List Test.ZM.ADT.Bit.K65149ce3b366.Bit)))
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( ByPattern a )
