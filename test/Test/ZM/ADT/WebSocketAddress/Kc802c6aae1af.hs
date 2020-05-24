{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.WebSocketAddress.Kc802c6aae1af (WebSocketAddress(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Bool.K306f1981b41c
import qualified Test.ZM.ADT.SocketAddress.Ke5d02571ce7b
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.Char.K066db52af145

data WebSocketAddress a =   WebSocketAddress {secure :: Test.ZM.ADT.Bool.K306f1981b41c.Bool,
                                              host :: Test.ZM.ADT.SocketAddress.Ke5d02571ce7b.SocketAddress a,
                                              path :: Test.ZM.ADT.List.Kb8cd13187198.List Test.ZM.ADT.Char.K066db52af145.Char}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( WebSocketAddress a )
