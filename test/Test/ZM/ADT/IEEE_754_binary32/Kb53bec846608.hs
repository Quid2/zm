{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.IEEE_754_binary32.Kb53bec846608 (IEEE_754_binary32(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Sign.K549f91f3b0ec
import qualified Test.ZM.ADT.MostSignificantFirst.K74e2b3b89941
import qualified Test.ZM.ADT.Bits8.K9e3b8c835fe9
import qualified Test.ZM.ADT.Bits23.K338888222364

data IEEE_754_binary32 =   IEEE_754_binary32 {sign :: Test.ZM.ADT.Sign.K549f91f3b0ec.Sign,
                                              exponent :: Test.ZM.ADT.MostSignificantFirst.K74e2b3b89941.MostSignificantFirst Test.ZM.ADT.Bits8.K9e3b8c835fe9.Bits8,
                                              fraction :: Test.ZM.ADT.MostSignificantFirst.K74e2b3b89941.MostSignificantFirst Test.ZM.ADT.Bits23.K338888222364.Bits23}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance Data.Model.Model IEEE_754_binary32
