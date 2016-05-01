{-# LANGUAGE StandaloneDeriving ,DeriveGeneric ,ScopedTypeVariables ,FlexibleContexts #-}
module Test.Data.Model where

import           Data.Model
import           Test.Data
import qualified Test.Data2 as Data2
import qualified Test.Data3 as Data3
import Data.Word
import GHC.Generics
import Data.Proxy
import Data.Typeable

-- instance Model Word8
-- instance Model Words
-- instance Model Ints

-- instance Model ADT
-- instance Model MM0
-- instance Model MM1
-- instance Model MM2
-- instance Model MM3
-- instance Model MM4
-- instance Model MM5
-- instance Model MM6

instance Model Void
instance Model Unit

instance Model N3
instance Model N
instance Model Un
instance Model D2
instance Model D4
instance Model A0
instance Model B0
instance Model C0
instance Model D0
instance Model E0
-- instance Model ()

instance Model a => Model (Phantom a)
instance Model a => Model (Data2.List a)
instance Model a => Model (Data3.List a)
instance Model a => Model (List a)
instance Model a => Model (Tree a)
instance (Model a, Model b, Model c) => Model (RR a b c)
instance Model Expr
-- instance (Model (f a),Typeable f,Model a) => Model (PerfectF f a)
-- instance (Model a) => Model (Free f a)
instance Model a => Model (Perfect a)
instance Model a => Model (Fork a)
instance Model a => Model (Forest a)
instance Model a => Model (Tr a)
instance Model t => Model (ForestD t)
instance (Model f,Model a) => Model (TrD f a)
instance Model a => Model (Forest2 a)
instance Model a => Model (Tr2 a) 
