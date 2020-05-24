{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Content.K957357183935 (Content(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.Char.K066db52af145

data Content a
             b =   TextMessage (Test.ZM.ADT.List.Kb8cd13187198.List Test.ZM.ADT.Char.K066db52af145.Char)
                 | AskSubSubjects
                 | Join
                 | Leave
                 | Ping
                 | AskUsers
                 | Users (Test.ZM.ADT.List.Kb8cd13187198.List a)
                 | AskHistory
                 | History (Test.ZM.ADT.List.Kb8cd13187198.List b)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance ( Data.Model.Model a,Data.Model.Model b ) => Data.Model.Model ( Content a b )
