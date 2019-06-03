{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.HostAddress.K64f93d94a73d (HostAddress(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.Char.K066db52af145

data HostAddress a =   IPAddress a
                     | DNSAddress (Test.ZM.ADT.List.Kb8cd13187198.List Test.ZM.ADT.Char.K066db52af145.Char)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( HostAddress a )
