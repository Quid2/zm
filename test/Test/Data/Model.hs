{-# LANGUAGE StandaloneDeriving ,DeriveGeneric ,ScopedTypeVariables #-}
module Test.Data.Model where

import           Data.Model
import           Test.Data
import qualified Test.Data2 as Data2
import Data.Word
import GHC.Generics
import Data.Proxy

--instance Model Char
-- instance Model Int

-- Provide models for Word8 .. using stand-in classes
-- instance Model Word8 where envType _ = envType (Proxy::Proxy Word8SI)
-- data Word8SI deriving Generic
-- instance Model Word8SI

-- TODO: Fix problems with types using symbolic constructors
-- instance Model a => Model [a] where
--   envType _ = envType (Proxy::Proxy (ListSI a))
-- data ListSI a deriving Generic
-- instance Model a => Model (ListSI a)


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
instance Model ()

instance Model a => Model (Phantom a)
instance Model a => Model (Data2.List a)
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

