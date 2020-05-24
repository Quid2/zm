{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Int16.K3dac6bd4fa9c (Int16(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.ZigZag.K03226796ede4
import qualified Test.ZM.ADT.Word16.K295e24d62fac

newtype Int16 =   Int16 (Test.ZM.ADT.ZigZag.K03226796ede4.ZigZag Test.ZM.ADT.Word16.K295e24d62fac.Word16)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance Data.Model.Model Int16
