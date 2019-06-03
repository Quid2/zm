{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Type.K7028aa556ebc (Type(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model

data Type a =   TypeCon a
              | TypeApp (Test.ZM.ADT.Type.K7028aa556ebc.Type a)
                        (Test.ZM.ADT.Type.K7028aa556ebc.Type a)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( Type a )
