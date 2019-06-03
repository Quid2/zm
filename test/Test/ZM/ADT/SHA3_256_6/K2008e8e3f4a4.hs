{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.SHA3_256_6.K2008e8e3f4a4 (SHA3_256_6(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Word8.Kb1f46a49c8f8

data SHA3_256_6 a =   SHA3_256_6 Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                                 Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                                 Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                                 Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                                 Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
                                 Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( SHA3_256_6 a )
