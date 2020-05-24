module Test.Data3.Flat
  ( module Test.Data3
  )
where
import           Flat
import           Test.Data3

instance Flat a => Flat (List a)
