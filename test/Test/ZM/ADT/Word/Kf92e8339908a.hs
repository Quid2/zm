{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Word.Kf92e8339908a (Word(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.LeastSignificantFirst.K20ffacc8f8c9
import qualified Test.ZM.ADT.NonEmptyList.Kbf2d1c86eb20
import qualified Test.ZM.ADT.MostSignificantFirst.K74e2b3b89941
import qualified Test.ZM.ADT.Word7.Kf4c946334a7e

newtype Word =   Word (Test.ZM.ADT.LeastSignificantFirst.K20ffacc8f8c9.LeastSignificantFirst (Test.ZM.ADT.NonEmptyList.Kbf2d1c86eb20.NonEmptyList (Test.ZM.ADT.MostSignificantFirst.K74e2b3b89941.MostSignificantFirst Test.ZM.ADT.Word7.Kf4c946334a7e.Word7)))
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Word
