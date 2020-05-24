
{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules#-}
module DocTest.Test where
import qualified DocTest
import Test.Tasty(TestTree,testGroup)
import Test

tests :: IO TestTree
tests = testGroup "Test" <$> sequence [  DocTest.test "src/Test.hs:43" "[]" (DocTest.asPrint(  ))]
