{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.LeastSignificantFirst.K20ffacc8f8c9 (LeastSignificantFirst(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model

newtype LeastSignificantFirst a =   LeastSignificantFirst a
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( LeastSignificantFirst a )
