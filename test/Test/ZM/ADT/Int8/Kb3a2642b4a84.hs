{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Int8.Kb3a2642b4a84 (Int8(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.ZigZag.K03226796ede4
import qualified Test.ZM.ADT.Word8.Kb1f46a49c8f8

newtype Int8 =   Int8 (Test.ZM.ADT.ZigZag.K03226796ede4.ZigZag Test.ZM.ADT.Word8.Kb1f46a49c8f8.Word8)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Int8
