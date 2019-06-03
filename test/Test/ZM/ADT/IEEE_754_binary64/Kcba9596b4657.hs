{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.IEEE_754_binary64.Kcba9596b4657 (IEEE_754_binary64(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Sign.K549f91f3b0ec
import qualified Test.ZM.ADT.MostSignificantFirst.K74e2b3b89941
import qualified Test.ZM.ADT.Bits11.K8ae75e67a616
import qualified Test.ZM.ADT.Bits52.Kf727da8aa8ad

data IEEE_754_binary64 =   IEEE_754_binary64 {sign :: Test.ZM.ADT.Sign.K549f91f3b0ec.Sign,
                                              exponent :: Test.ZM.ADT.MostSignificantFirst.K74e2b3b89941.MostSignificantFirst Test.ZM.ADT.Bits11.K8ae75e67a616.Bits11,
                                              fraction :: Test.ZM.ADT.MostSignificantFirst.K74e2b3b89941.MostSignificantFirst Test.ZM.ADT.Bits52.Kf727da8aa8ad.Bits52}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model IEEE_754_binary64
