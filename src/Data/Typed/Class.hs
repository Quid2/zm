{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
-- |Typed class
module Data.Typed.Class(
  --Typed(..)
  absType
  ) where

import           Data.Model
import           Data.Typed.Abs
import           Data.Typed.Types

{- |
Defined for the purpose of being overwritten

instance {-# OVERLAPPING #-} Typed a => Typed (List a) where
   absType _ = TypeApp (TypeCon (AbsRef (SHA3_256_6 104 188 123 93 245 148))) (absType (Proxy :: Proxy a))

But this wont work as we

We need types to be instances of Model 
-}
-- class Typed a where
--   -- |Return the absolute type corresponding to the given type
--   absType :: Proxy a -> Type AbsRef

  -- requires explicit Typed instance
  --default absType :: Model a => Proxy a -> Type AbsRef
  --absType = typeName . absTypeModel

-- instance Typed a => Typed [a] where absType _ = TypeApp (TypeCon (AbsRef (SHA3_256_6 104 188 123 93 245 148))) (absType (Proxy :: Proxy a))


--instance {-# OVERLAPPABLE #-} Model a => Typed a where absType = typeName . absTypeModel

absType :: Model a => Proxy a -> Type AbsRef
absType = typeName . absTypeModel
