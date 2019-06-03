{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.ADTRef.K07b1b045ac3c (ADTRef(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Word8.Kb1f46a49c8f8

data ADTRef a =   Var Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                | Rec
                | Ext a
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( ADTRef a )
