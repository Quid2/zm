{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.IP4Address.K6cb2ee3ac409 (IP4Address(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Word8.Kb1f46a49c8f8

data IP4Address =   IP4Address Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                               Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                               Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                               Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model IP4Address
