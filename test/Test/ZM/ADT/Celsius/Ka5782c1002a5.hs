{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Celsius.Ka5782c1002a5 (Celsius(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.IEEE_754_binary32.Kb53bec846608

newtype Celsius =   Celsius Test.ZM.ADT.IEEE_754_binary32.Kb53bec846608.IEEE_754_binary32
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Celsius
