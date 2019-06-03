{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.User.K0e1df25dc480 (User(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.Char.K066db52af145

newtype User =   User {userName :: Test.ZM.ADT.List.Kb8cd13187198.List Test.ZM.ADT.Char.K066db52af145.Char}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model User
