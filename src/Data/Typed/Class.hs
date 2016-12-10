{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Typed.Class(
  Typed(..)
  ) where

import           Data.Model
import           Data.Typed.Abs
import           Data.Typed.Types

{- |
instance {-# OVERLAPPING #-} Typed a => Typed (List a) where
   absType _ = TypeApp (TypeCon (AbsRef (SHA3_256_6 104 188 123 93 245 148))) (absType (Proxy :: Proxy a))
-}
class Typed a where
  -- |Return the absolute type corresponding to the given Proxy
  absType :: Proxy a -> Type AbsRef

instance {-# OVERLAPPABLE #-} Model a => Typed a where absType = typeName . absTypeModel

