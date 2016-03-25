{-# LANGUAGE DeriveGeneric, ScopedTypeVariables ,CPP #-}
module Data.Typed.Instances where

-- import Data.Typed.Class
import Data.Model
-- import qualified Data.Model.Quid2 as Q
import Data.Typed.Types
import qualified QQ
import Data.Word
import Data.Int
import Data.Flat

#include "MachDeps.h"

instance Model Char where envType _ = envType (Proxy::Proxy QQ.Char)

instance Model Word8 where envType _ = envType (Proxy::Proxy QQ.Word8)
instance Model Word16 where envType _ = envType (Proxy::Proxy QQ.Word16)
instance Model Word32 where envType _ = envType (Proxy::Proxy QQ.Word32)
instance Model Word64 where envType _ = envType (Proxy::Proxy QQ.Word64)

instance Model Int8 where envType _ = envType (Proxy::Proxy QQ.Int8)
instance Model Int16 where envType _ = envType (Proxy::Proxy QQ.Int16)
instance Model Int32 where envType _ = envType (Proxy::Proxy QQ.Int32)
instance Model Int64 where envType _ = envType (Proxy::Proxy QQ.Int64)
instance Model Integer where envType _ = envType (Proxy::Proxy QQ.Integer)

#if WORD_SIZE_IN_BITS == 64
instance Model Word where envType _ = envType (Proxy::Proxy QQ.Word64)
instance Model Int where envType _ = envType (Proxy::Proxy QQ.Int64)
#elif WORD_SIZE_IN_BITS == 32
instance Model Word where envType _ = envType (Proxy::Proxy QQ.Word32)
instance Model Int where envType _ = envType (Proxy::Proxy QQ.Int32)
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif

instance (Model a,Model b) => Model (ADT a b)
instance Model a => Model (ConTree a)
instance Model a => Model (Ref a)
instance Model ADTRef
instance Model a => Model (Type a)
instance Model a => Model (TypeRef a)

data Tuple2 a b = Tuple2 a b deriving (Eq, Ord, Show, Generic)
instance (Model a,Model b) => Model (Tuple2 a b)

data Tuple3 a b c = Tuple3 a b c deriving (Eq, Ord, Show, Generic)
instance (Model a,Model b,Model c) => Model (Tuple3 a b c)

data Tuple4 a b c d = Tuple4 a b c d deriving (Eq, Ord, Show, Generic)
instance (Model a,Model b,Model c,Model d) => Model (Tuple4 a b c d)

instance {-# OVERLAPPABLE #-} (Model a,Model b) => Model (a,b) where envType _ = envType (Proxy::Proxy (Tuple2 a b))
instance {-# OVERLAPPABLE #-} (Model a,Model b,Model c) => Model (a,b,c) where envType _ = envType (Proxy::Proxy (Tuple3 a b c))
instance {-# OVERLAPPABLE #-} (Model a,Model b,Model c,Model d) => Model (a,b,c,d) where envType _ = envType (Proxy::Proxy (Tuple4 a b c d))

instance {-# OVERLAPPABLE #-} Model a => Model [a] where envType _ = envType (Proxy::Proxy (QQ.List a))

instance (Flat a,Flat b) => Flat (ADT a b)
instance Flat a => Flat (ConTree a)
instance Flat a => Flat (Ref a)
instance Flat ADTRef
instance Flat a => Flat (Type a)
instance Flat a => Flat (TypeRef a)
instance Flat a => Flat (NonEmptyList a)
