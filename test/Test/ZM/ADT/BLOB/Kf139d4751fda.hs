{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.BLOB.Kf139d4751fda (BLOB(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Bytes.Kf8844385a443

data BLOB a =   BLOB {encoding :: a,
                      content :: Test.ZM.ADT.Bytes.Kf8844385a443.Bytes}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( BLOB a )
