{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.HostPort.K0ab5ac6303b9 (HostPort(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Word16.K295e24d62fac

newtype HostPort =   HostPort {port :: Test.ZM.ADT.Word16.K295e24d62fac.Word16}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model HostPort
