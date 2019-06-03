{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Int64.Kfb94cb4d4ede (Int64(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.ZigZag.K03226796ede4
import qualified Test.ZM.ADT.Word64.K50d018f7593a

newtype Int64 =   Int64 (Test.ZM.ADT.ZigZag.K03226796ede4.ZigZag Test.ZM.ADT.Word64.K50d018f7593a.Word64)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Int64
