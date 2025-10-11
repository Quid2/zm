{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZM.Parser.ParseF where

import Control.Applicative
import Control.Monad.Fix
import Data.Comp (Term)
import Data.Comp.Ops
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Text.Earley

grammar :: (Prod r e t a -> Prod r e t a) -> Grammar r (Prod r e t a)
grammar p = mfix $ rule . p

-- Can be generalized to any recursive fix?
class ParseF (f :: Type -> Type) t where
  -- parse :: (f :<: t) => Proxy (f ()) -> Prod r String String (Term t) -> Prod r String String (Term t)
  parse :: (f :<: t) => Prod r String String (Term t) -> Prod r String String (Term t)

instance (ParseF f t, ParseF g t, f :<: t, g :<: t) => ParseF ((:+:) f g) t where
  parse top = parse @f top <|> parse @g top

