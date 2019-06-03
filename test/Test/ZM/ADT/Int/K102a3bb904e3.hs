{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Int.K102a3bb904e3 (Int(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.ZigZag.K03226796ede4
import qualified Test.ZM.ADT.Word.Kf92e8339908a

newtype Int =   Int (Test.ZM.ADT.ZigZag.K03226796ede4.ZigZag Test.ZM.ADT.Word.Kf92e8339908a.Word)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Int
