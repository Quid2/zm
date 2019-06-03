{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Word64.K50d018f7593a (Word64(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Word.Kf92e8339908a

newtype Word64 =   Word64 Test.ZM.ADT.Word.Kf92e8339908a.Word
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Word64
