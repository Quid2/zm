{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Time.Kf3f0f3c453f7 (Time(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Int.K102a3bb904e3
import qualified Test.ZM.ADT.Word32.K2412799c99f1

data Time =   Time {utcDay :: Test.ZM.ADT.Int.K102a3bb904e3.Int,
                    utcSecs :: Test.ZM.ADT.Word32.K2412799c99f1.Word32}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Time
