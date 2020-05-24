module Main where
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified DocTest.Test

main = (testGroup "DocTests" <$> sequence [DocTest.Test.tests]) >>= defaultMain
