{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Subject.Kfced5b0f3c1f (Subject(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.Char.K066db52af145

newtype Subject =   Subject (Test.ZM.ADT.List.Kb8cd13187198.List (Test.ZM.ADT.List.Kb8cd13187198.List Test.ZM.ADT.Char.K066db52af145.Char))
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Subject
