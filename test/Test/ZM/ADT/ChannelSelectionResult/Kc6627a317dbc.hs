{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.ChannelSelectionResult.Kc6627a317dbc (ChannelSelectionResult(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.Char.K066db52af145

data ChannelSelectionResult a =   Success
                                | Failure {reason :: Test.ZM.ADT.List.Kb8cd13187198.List Test.ZM.ADT.Char.K066db52af145.Char}
                                | RetryAt a
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( ChannelSelectionResult a )
