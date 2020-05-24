{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.UnicodeSymbol.K801030ef543c (UnicodeSymbol(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Char.K066db52af145

newtype UnicodeSymbol =   UnicodeSymbol Test.ZM.ADT.Char.K066db52af145.Char
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance Data.Model.Model UnicodeSymbol
