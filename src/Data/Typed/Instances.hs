{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typed.Instances(
  Tuple2(..),Tuple3(..),Tuple4(..),Tuple5(..),Tuple6(..),Tuple7(..),Tuple8(..),Tuple9(..)
  ) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Flat
import           Data.Int
import           Data.Model
import           Data.Text            (Text)
import qualified Data.Typed.PrimTypes as P
import           Data.Typed.Types
import           Data.Word
import           Debug.Trace

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64
instance Model Word where envType _ = envType (Proxy::Proxy P.Word64)
instance Model Int where envType _ = envType (Proxy::Proxy P.Int64)
#elif WORD_SIZE_IN_BITS == 32
instance Model Word where envType _ = envType (Proxy::Proxy P.Word32)
instance Model Int where envType _ = envType (Proxy::Proxy P.Int32)
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif

--instance Model Word8 where envType _ = envType (Proxy::Proxy P.Word8)
instance Model Word16 where envType _ = envType (Proxy::Proxy P.Word16)
instance Model Word32 where envType _ = envType (Proxy::Proxy P.Word32)
instance Model Word64 where envType _ = envType (Proxy::Proxy P.Word64)

instance Model Int8 where envType _ = envType (Proxy::Proxy P.Int8)
instance Model Int16 where envType _ = envType (Proxy::Proxy P.Int16)
instance Model Int32 where envType _ = envType (Proxy::Proxy P.Int32)
instance Model Int64 where envType _ = envType (Proxy::Proxy P.Int64)
instance Model Integer where envType _ = envType (Proxy::Proxy P.Integer)

instance Model Char where envType _ = envType (Proxy::Proxy P.Char)

instance (Model a,Model b) => Model (ADT a b)
instance Model a => Model (ConTree a)
instance Model a => Model (Ref a)
instance Model ADTRef
instance Model a => Model (Type a)
instance Model a => Model (TypeRef a)
-- instance Model AbsoluteType

instance Model UTF8Encoding
instance Model FlatEncoding

instance Model Filler
instance Model a => Model (PreAligned a)

-- BUG, return as 'Array' (as these are zero-kinded, their abs type is assumed to be the same?)
-- instance AsType B.ByteString where envType _ = envType (Proxy::Proxy (Tuple2 Bool ()))
-- instance Model B.ByteString where envType _ = envType (Proxy::Proxy (Tuple2 Bool ()))
-- NOTE: need to overload AsType as well as arity of ByteString =/= Array Word8
instance Model B.ByteString where envType _ = envType (Proxy::Proxy (Array Word8))
instance {-# OVERLAPPING #-} AsType (Typ B.ByteString) where asType _ = asType (undefined::Ana (Array Word8))

instance Model L.ByteString where envType _ = envType (Proxy::Proxy (Array Word8))
instance {-# OVERLAPPING #-} AsType (Typ L.ByteString) where asType _ = asType (undefined::Ana (Array Word8))

--instance Model Text where envType _ = envType (Proxy::Proxy P.String)
instance Model Text where envType _ = envType (Proxy::Proxy (BLOB UTF8Encoding))
instance {-# OVERLAPPING #-} AsType (Typ Text) where asType _ = asType (undefined::Ana (BLOB UTF8Encoding))

instance Model e => Model (BLOB e)

data Tuple2 a b = Tuple2 a b deriving (Eq, Ord, Show, Generic)
instance (Model a,Model b) => Model (Tuple2 a b)
instance (Model a,Model b) => Model (a,b) where envType _ = envType (Proxy::Proxy (Tuple2 a b))

data Tuple3 a b c = Tuple3 a b c deriving (Eq, Ord, Show, Generic)
instance (Model a,Model b,Model c) => Model (Tuple3 a b c)
instance (Model a,Model b,Model c) => Model (a,b,c) where envType _ = envType (Proxy::Proxy (Tuple3 a b c))

data Tuple4 a b c d = Tuple4 a b c d deriving (Eq, Ord, Show, Generic)
instance (Model a,Model b,Model c,Model d) => Model (Tuple4 a b c d)
instance (Model a,Model b,Model c,Model d) => Model (a,b,c,d) where envType _ = envType (Proxy::Proxy (Tuple4 a b c d))

data Tuple5 a1 a2 a3 a4 a5 = Tuple5 a1 a2 a3 a4 a5 deriving (Eq, Ord, Show, Generic)
instance (Model a1,Model a2,Model a3,Model a4,Model a5) => Model (Tuple5 a1 a2 a3 a4 a5)
instance (Model a1,Model a2,Model a3,Model a4,Model a5) => Model (a1,a2,a3,a4,a5) where envType _ = envType (Proxy::Proxy (Tuple5 a1 a2 a3 a4 a5))

data Tuple6 a1 a2 a3 a4 a5 a6 = Tuple6 a1 a2 a3 a4 a5 a6 deriving (Eq, Ord, Show, Generic)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6) => Model (Tuple6 a1 a2 a3 a4 a5 a6)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6) => Model (a1,a2,a3,a4,a5,a6) where envType _ = envType (Proxy::Proxy (Tuple6 a1 a2 a3 a4 a5 a6))

data Tuple7 a1 a2 a3 a4 a5 a6 a7= Tuple7 a1 a2 a3 a4 a5 a6 a7 deriving (Eq, Ord, Show, Generic)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7) => Model (Tuple7 a1 a2 a3 a4 a5 a6 a7)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7) => Model (a1,a2,a3,a4,a5,a6,a7) where envType _ = envType (Proxy::Proxy (Tuple7 a1 a2 a3 a4 a5 a6 a7))

data Tuple8 a1 a2 a3 a4 a5 a6 a7 a8 = Tuple8 a1 a2 a3 a4 a5 a6 a7 a8 deriving (Eq, Ord, Show, Generic)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7,Model a8) => Model (Tuple8 a1 a2 a3 a4 a5 a6 a7 a8)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7,Model a8) => Model (a1,a2,a3,a4,a5,a6,a7,a8) where envType _ = envType (Proxy::Proxy (Tuple8 a1 a2 a3 a4 a5 a6 a7 a8))

data Tuple9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = Tuple9 a1 a2 a3 a4 a5 a6 a7 a8 a9 deriving (Eq, Ord, Show, Generic)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7,Model a8,Model a9) => Model (Tuple9 a1 a2 a3 a4 a5 a6 a7 a8 a9)
instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7,Model a8,Model a9) => Model (a1,a2,a3,a4,a5,a6,a7,a8,a9) where envType _ = envType (Proxy::Proxy (Tuple9 a1 a2 a3 a4 a5 a6 a7 a8 a9))

-- [a] == data String = String (List a)

instance Model a => Model [a] where envType _ = envType (Proxy::Proxy (P.List a))

 -- instance {-# OVERLAPPABLE #-} Model a => Model [a] where envType _ = envType (Proxy::Proxy (P.List a))

-- last resort :-(
-- instance {-# OVERLAPPABLE #-} Model a => Model [a] where
--   envType _ = do
--      t <- envType (Proxy::Proxy a)
--      -- (show t) -- !! TypeCon (TypVar 0)
--      case traceShowId t of
--        -- Char
--        TypeCon (TypRef (QualName _ _ "Char")) -> envType (Proxy::Proxy P.String)
--        _ -> envType (Proxy::Proxy (P.List a))

-- Why is this ignored?
-- Because [Char] is analysed as [TypVar..]
-- instance {-# OVERLAPS #-} Model [Char] where envType _ = envType (Proxy::Proxy Bool)
-- instance {-# OVERLAPS #-} Model [Word8] where envType _ = envType (Proxy::Proxy Bool)
-- instance {-# OVERLAPS #-} Model [Char] where envType _ = envType (Proxy::Proxy P.String)
-- instance {-# OVERLAPS #-} Model [Char] where envType p = addCT (Proxy::Proxy P.String)
-- instance Model (Char,Char) where envType _ = envType (Proxy::Proxy (P.Word8))

-- instance Typed P.String
-- instance Model a => Typed (P.List a)

-- x = absoluteType (Proxy::Proxy P.String)

-- --- instance {-# OVERLAPPABLE #-} Typed a => Typed [a] where absoluteType _ = absoluteType (Proxy::Proxy (P.List a))

-- instance Typed [Char] where absoluteType _ = absoluteType (Proxy::Proxy P.String)

instance (Flat a,Flat b) => Flat (ADT a b)
instance Flat a => Flat (ConTree a)
instance Flat a => Flat (Ref a)
instance Flat ADTRef
instance Flat a => Flat (Type a)
instance Flat a => Flat (TypeRef a)
instance Flat a => Flat (NonEmptyList a)
-- instance Flat AbsoluteType
