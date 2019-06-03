{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Bits11.K8ae75e67a616 (Bits11(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Bit.K65149ce3b366

data Bits11 =   Bits11 {bit0 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                        bit1 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                        bit2 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                        bit3 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                        bit4 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                        bit5 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                        bit6 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                        bit7 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                        bit8 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                        bit9 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                        bit10 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Bits11
