{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.III.K4cf5dd973ae4 (III(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Int8.Kb3a2642b4a84
import qualified Test.ZM.ADT.Int16.K3dac6bd4fa9c
import qualified Test.ZM.ADT.Int64.Kfb94cb4d4ede
import qualified Test.ZM.ADT.IEEE_754_binary32.Kb53bec846608
import qualified Test.ZM.ADT.IEEE_754_binary64.Kcba9596b4657
import qualified Test.ZM.ADT.Int.K102a3bb904e3

data III =   III {w8 :: Test.ZM.ADT.Int8.Kb3a2642b4a84.Int8,
                  w16 :: Test.ZM.ADT.Int16.K3dac6bd4fa9c.Int16,
                  w :: Test.ZM.ADT.Int64.Kfb94cb4d4ede.Int64,
                  i8 :: Test.ZM.ADT.Int8.Kb3a2642b4a84.Int8,
                  i :: Test.ZM.ADT.Int64.Kfb94cb4d4ede.Int64,
                  f :: Test.ZM.ADT.IEEE_754_binary32.Kb53bec846608.IEEE_754_binary32,
                  d :: Test.ZM.ADT.IEEE_754_binary64.Kcba9596b4657.IEEE_754_binary64,
                  ii :: Test.ZM.ADT.Int.K102a3bb904e3.Int}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance Data.Model.Model III
