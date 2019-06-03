{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.SHAKE128_48.K9f214799149b (SHAKE128_48(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Word8.Kb1f46a49c8f8

data SHAKE128_48 a =   SHAKE128_48 Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                                   Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                                   Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                                   Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                                   Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                                   Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( SHAKE128_48 a )
