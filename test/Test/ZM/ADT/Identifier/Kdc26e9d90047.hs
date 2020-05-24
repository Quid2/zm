{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Identifier.Kdc26e9d90047 (Identifier(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.UnicodeLetter.K3878b3580fc5
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.UnicodeLetterOrNumberOrLine.K33445520c45a
import qualified Test.ZM.ADT.NonEmptyList.Kbf2d1c86eb20
import qualified Test.ZM.ADT.UnicodeSymbol.K801030ef543c

data Identifier =   Name Test.ZM.ADT.UnicodeLetter.K3878b3580fc5.UnicodeLetter
                         (Test.ZM.ADT.List.Kb8cd13187198.List Test.ZM.ADT.UnicodeLetterOrNumberOrLine.K33445520c45a.UnicodeLetterOrNumberOrLine)
                  | Symbol (Test.ZM.ADT.NonEmptyList.Kbf2d1c86eb20.NonEmptyList Test.ZM.ADT.UnicodeSymbol.K801030ef543c.UnicodeSymbol)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance Data.Model.Model Identifier
