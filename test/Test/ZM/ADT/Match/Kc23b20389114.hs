{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Match.Kc23b20389114 (Match(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.Type.K7028aa556ebc
import qualified Test.ZM.ADT.AbsRef.K4bbd38587b9e

data Match a =   MatchValue a
               | MatchAny (Test.ZM.ADT.Type.K7028aa556ebc.Type Test.ZM.ADT.AbsRef.K4bbd38587b9e.AbsRef)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance ( Data.Model.Model a ) => Data.Model.Model ( Match a )
