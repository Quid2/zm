{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.TypedBLOB.K614edd84c8bd (TypedBLOB(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Type.K7028aa556ebc
import qualified Test.ZM.ADT.AbsRef.K4bbd38587b9e
import qualified Test.ZM.ADT.BLOB.Kf139d4751fda
import qualified Test.ZM.ADT.FlatEncoding.K982148c09ddb

data TypedBLOB =   TypedBLOB (Test.ZM.ADT.Type.K7028aa556ebc.Type Test.ZM.ADT.AbsRef.K4bbd38587b9e.AbsRef)
                             (Test.ZM.ADT.BLOB.Kf139d4751fda.BLOB Test.ZM.ADT.FlatEncoding.K982148c09ddb.FlatEncoding)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance Data.Model.Model TypedBLOB
