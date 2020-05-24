{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.SocketAddress.Ke5d02571ce7b (SocketAddress(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.HostAddress.K64f93d94a73d
import qualified Test.ZM.ADT.HostPort.K0ab5ac6303b9

data SocketAddress a =   SocketAddress {socketAddress :: Test.ZM.ADT.HostAddress.K64f93d94a73d.HostAddress a,
                                        socketPort :: Test.ZM.ADT.HostPort.K0ab5ac6303b9.HostPort}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( SocketAddress a )
