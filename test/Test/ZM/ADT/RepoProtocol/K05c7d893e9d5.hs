{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.RepoProtocol.K05c7d893e9d5 (RepoProtocol(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.ADT.K3e8257255cbf
import qualified Test.ZM.ADT.Identifier.Kdc26e9d90047
import qualified Test.ZM.ADT.ADTRef.K07b1b045ac3c
import qualified Test.ZM.ADT.AbsRef.K4bbd38587b9e
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.Tuple2.Ka5583bf3ad34

data RepoProtocol =   Record (Test.ZM.ADT.ADT.K3e8257255cbf.ADT Test.ZM.ADT.Identifier.Kdc26e9d90047.Identifier
                                                                Test.ZM.ADT.Identifier.Kdc26e9d90047.Identifier
                                                                (Test.ZM.ADT.ADTRef.K07b1b045ac3c.ADTRef Test.ZM.ADT.AbsRef.K4bbd38587b9e.AbsRef))
                    | Solve Test.ZM.ADT.AbsRef.K4bbd38587b9e.AbsRef
                    | Solved Test.ZM.ADT.AbsRef.K4bbd38587b9e.AbsRef
                             (Test.ZM.ADT.ADT.K3e8257255cbf.ADT Test.ZM.ADT.Identifier.Kdc26e9d90047.Identifier
                                                                Test.ZM.ADT.Identifier.Kdc26e9d90047.Identifier
                                                                (Test.ZM.ADT.ADTRef.K07b1b045ac3c.ADTRef Test.ZM.ADT.AbsRef.K4bbd38587b9e.AbsRef))
                    | AskDataTypes
                    | KnownDataTypes (Test.ZM.ADT.List.Kb8cd13187198.List (Test.ZM.ADT.Tuple2.Ka5583bf3ad34.Tuple2 Test.ZM.ADT.AbsRef.K4bbd38587b9e.AbsRef
                                                                                                                   (Test.ZM.ADT.ADT.K3e8257255cbf.ADT Test.ZM.ADT.Identifier.Kdc26e9d90047.Identifier
                                                                                                                                                      Test.ZM.ADT.Identifier.Kdc26e9d90047.Identifier
                                                                                                                                                      (Test.ZM.ADT.ADTRef.K07b1b045ac3c.ADTRef Test.ZM.ADT.AbsRef.K4bbd38587b9e.AbsRef))))
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance Data.Model.Model RepoProtocol
