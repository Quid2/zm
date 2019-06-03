{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Message.K551d9f2adb72 (Message(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Data.Flat
import qualified Data.Model
import qualified Test.ZM.ADT.User.K0e1df25dc480
import qualified Test.ZM.ADT.Subject.Kfced5b0f3c1f
import qualified Test.ZM.ADT.Content.K957357183935

data Message =   Message {fromUser :: Test.ZM.ADT.User.K0e1df25dc480.User,
                          subject :: Test.ZM.ADT.Subject.Kfced5b0f3c1f.Subject,
                          content :: Test.ZM.ADT.Content.K957357183935.Content Test.ZM.ADT.User.K0e1df25dc480.User
                                                                               Test.ZM.ADT.Message.K551d9f2adb72.Message}
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)
instance Data.Model.Model Message
