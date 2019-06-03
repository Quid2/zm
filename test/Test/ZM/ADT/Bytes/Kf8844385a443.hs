{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Bytes.Kf8844385a443 (Bytes(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.PreAligned.Kb2f28cf37d12
import qualified Test.ZM.ADT.Array.K2e8b4519aeaa
import qualified Test.ZM.ADT.Word8.Kb1f46a49c8f8

newtype Bytes =   Bytes (Test.ZM.ADT.PreAligned.Kb2f28cf37d12.PreAligned (Test.ZM.ADT.Array.K2e8b4519aeaa.Array Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8))
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Bytes
