module Test.Data3.Flat(module Test.Data3) where
import Data.Flat
import Test.Data3

instance Flat a => Flat (List a)
