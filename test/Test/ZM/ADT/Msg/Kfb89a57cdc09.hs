{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Msg.Kfb89a57cdc09 (Msg(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Test.ZM.ADT.List.Kb8cd13187198
import qualified Test.ZM.ADT.Char.K066db52af145
import qualified Test.ZM.ADT.Subject.Kfced5b0f3c1f
import qualified Test.ZM.ADT.Content.K1ba230d92eb8

data Msg =   Msg {fromUser :: Test.ZM.ADT.List.Kb8cd13187198.List Test.ZM.ADT.Char.K066db52af145.Char,
                  subject :: Test.ZM.ADT.Subject.Kfced5b0f3c1f.Subject,
                  content :: Test.ZM.ADT.Content.K1ba230d92eb8.Content}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat)
instance Data.Model.Model Msg
