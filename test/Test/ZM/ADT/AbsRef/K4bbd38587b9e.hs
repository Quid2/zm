{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.AbsRef.K4bbd38587b9e (AbsRef(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.SHAKE128_48.K9f214799149b
import qualified Test.ZM.ADT.ADT.K3e8257255cbf
import qualified Test.ZM.ADT.Identifier.Kdc26e9d90047
import qualified Test.ZM.ADT.ADTRef.K07b1b045ac3c

newtype AbsRef =   AbsRef (Test.ZM.ADT.SHAKE128_48.K9f214799149b.SHAKE128_48 (Test.ZM.ADT.ADT.K3e8257255cbf.ADT Test.ZM.ADT.Identifier.Kdc26e9d90047.Identifier
                                                                                                                Test.ZM.ADT.Identifier.Kdc26e9d90047.Identifier
                                                                                                                (Test.ZM.ADT.ADTRef.K07b1b045ac3c.ADTRef Test.ZM.ADT.AbsRef.K4bbd38587b9e.AbsRef)))
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance Data.Model.Model AbsRef
