{-# LANGUAGE ScopedTypeVariables #-}
module ZM.Coerce(coerce) where

import           Data.Model    (Model, Proxy (..))
import           Unsafe.Coerce
import           ZM.Abs        (absType)

-- |Safely coerce between two types that have the same ZM type
-- ZM type identity does not correspond necessarily to the same memory layout
-- Many types that share the same in memory structure have different ZM types.
-- ? And unfortunately is possible to give the same ZM type to types with different memory layouts
coerce :: forall a b . (Model a , Model b) => a -> Maybe b
coerce a | absType (Proxy :: Proxy a) == absType (Proxy::Proxy b) = Just (unsafeCoerce a)
         | otherwise = Nothing
