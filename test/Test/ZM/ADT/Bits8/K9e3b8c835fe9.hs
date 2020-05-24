{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Bits8.K9e3b8c835fe9 (Bits8(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Bit.K65149ce3b366

data Bits8 =   Bits8 {bit0 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                      bit1 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                      bit2 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                      bit3 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                      bit4 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                      bit5 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                      bit6 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit,
                      bit7 :: Test.ZM.ADT.Bit.K65149ce3b366.Bit}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance Data.Model.Model Bits8
