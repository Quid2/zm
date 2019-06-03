{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.NonEmptyList.Kbf2d1c86eb20 (NonEmptyList(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model

data NonEmptyList a =   Elem a
                      | Cons a (Test.ZM.ADT.NonEmptyList.Kbf2d1c86eb20.NonEmptyList a)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( NonEmptyList a )
