{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZM.Coerce (coerce) where

import Data.Model (Model, Proxy (..))
import Data.Type.Equality
import Flat
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import ZM.Abs (absType)
import ZM.Pretty
import ZM.Type.Char
import ZM.Type.Float32

{-
Safely?? coerce between two types that have the same ZM type and the same memory layout

ZM type identity does not correspond necessarily to the same memory layout

Many types that share the same in memory structure have different ZM types.

>>> import           ZM.Type.Char
>>> import           ZM.Type.Float32

>>> import ZM.Type.Float32
>>> absType (Proxy :: Proxy Float) == absType (Proxy :: Proxy IEEE_754_binary32)
True

Also crashes::

>>> coerceFlat (11.1::Float) :: Maybe IEEE_754_binary32

>>> absType (Proxy :: Proxy Float)
TypeCon (AbsRef (SHAKE128_48 181 59 236 132 102 8))

>>> absType (Proxy :: Proxy IEEE_754_binary32)
TypeCon (AbsRef (SHAKE128_48 181 59 236 132 102 8))

>>> import ZM.Type.Bit

-}

{-
And unfortunately is possible to give the same ZM type to types with different memory layouts

For example, this will SegFault as a Char is equivalent but has a very different memory layout from ZM.Type.Char :

>>> import ZM.Type.Char
>>> coerce 'a' :: Maybe ZM.Type.Char.Char
-}
coerce :: forall a b. (Flat a, Flat b, Model a, Model b) => a -> Maybe b
coerce a
    | absType (Proxy :: Proxy a) == absType (Proxy :: Proxy b) =
        case unflat (flat a) :: Decoded b of
            Right b -> Just b
            Left _ -> error "coerce:impossible"
    | otherwise = Nothing

-- c1 = c (32.23::Float) :: IEEE_754_binary32

-- -- c2 = c 'a' :: IEEE_754_binary32

-- c :: forall a b . (Flat a, Flat b, Model a , Model b,ZZ a ~ ZZ b) => a -> b
-- -- c :: forall a b . (Flat a, Flat b) => a -> b
-- c  a = case unflat (flat a) :: Decoded b of
--                 Right b -> b
--                 Left _  -> error "impossible"
-- type family ZZ t  where
--     ZZ Float = ZM "IEEE_754_binary32"
--     ZZ IEEE_754_binary32 = ZM "IEEE_754_binary32"
--     ZZ Prelude.Char = ZM "Char"

-- asym :: forall a. Model a => a -> SomeSymbol
-- asym _ = someSymbolVal $ show $ absType (Proxy::Proxy a)

-- class Model a => X a  where
--     x :: ZM s
-- data ZM (zm::Symbol) = ZM
