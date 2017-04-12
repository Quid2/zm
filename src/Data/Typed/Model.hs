{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Instances of Model for primitive types (Words,Ints,Char,[a],tuples,Text,ByteString..)
module Data.Typed.Model (Bytes) where

import           Data.BLOB
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Short as SBS
import           Data.Flat
import qualified Data.Int              as H
import           Data.List.NonEmpty
import qualified Data.Map              as M
import           Data.Model
import qualified Data.Sequence         as S
import           Data.Text             (Text)
import           Data.Typed.Defs
import           Data.Typed.Defs2
import           Data.Typed.Generate
import           Data.Typed.Types
import qualified Data.Word             as H
import           Prelude               (Eq, Ord, Show, error, undefined)
import qualified Prelude               as H
import           Type.Analyse

-- |Instances for primitive types
#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64
instance Model H.Word where envType _ = envType (Proxy::Proxy Word64)
instance Model H.Int where envType _ = envType (Proxy::Proxy Int64)
#elif WORD_SIZE_IN_BITS == 32
instance Model H.Word where envType _ = envType (Proxy::Proxy Word32)
instance Model H.Int where envType _ = envType (Proxy::Proxy Int32)
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif

instance Model H.Word16 where envType _ = envType (Proxy::Proxy Word16)
instance Model H.Word32 where envType _ = envType (Proxy::Proxy Word32)
instance Model H.Word64 where envType _ = envType (Proxy::Proxy Word64)
instance Model H.Int8 where envType _ = envType (Proxy::Proxy Int8)
instance Model H.Int16 where envType _ = envType (Proxy::Proxy Int16)
instance Model H.Int32 where envType _ = envType (Proxy::Proxy Int32)
instance Model H.Int64 where envType _ = envType (Proxy::Proxy Int64)
instance Model H.Integer where envType _ = envType (Proxy::Proxy Int)

instance Model H.Float where envType _ = envType (Proxy::Proxy IEEE_754_binary32)
instance Model H.Double where envType _ = envType (Proxy::Proxy IEEE_754_binary64)

instance Model H.Char where envType _ = envType (Proxy::Proxy Char)
instance Model a => Model [a] where envType _ = envType (Proxy::Proxy (List a))

-- ZM definitions

-- data Word7 = U0 .. U127

-- data Word8 = U0 | U1 .. | U255

-- data NonEmptyList a = Elem a | Cons a (NonEmptyList a)

-- data Map a b = Map (List (Tuple2 a b))

{- |An unsigned integer of arbitrary length
encoded as a non empty list of Word7 words
with least significant word first
and, inside each word, most significant bit first
VarWord is a sequence of Word7, where every byte except the last one has the most significant bit (msb) set.
Example:
3450 :: Word16/32/64.. = 0000110101111010 = 11010(26) 1111010(122) coded as:
Word16 (Cons V122 (Elem V26))
so Least Significant Byte first.
-}
data Word = Word (LeastSignificantFirst (NonEmptyList (MostSignificantFirst Word7)))
  deriving (Eq, Ord, Show, Generic, Model)

data Word16 = Word16 Word
  deriving (Eq, Ord, Show, Generic, Model)

data Word32 = Word32 Word
  deriving (Eq, Ord, Show, Generic, Model)

data Word64 = Word64 Word
  deriving (Eq, Ord, Show, Generic, Model)

-- ZigZag Encoding
-- data Int8 = Z | N1 |P1| N2 |P2 | N3 .. |P127 | N128

-- Ints and Integer are encoded as
-- Encoded as ZigZag Word16

-- ZigZag indicates ZigZag encoding
-- data ZigZag a = ZigZag a

data Int8 = Int8 (ZigZag H.Word8)
  deriving (Eq, Ord, Show, Generic, Model)

data Int16 = Int16 (ZigZag Word16)
  deriving (Eq, Ord, Show, Generic, Model)

data Int32 = Int32 (ZigZag Word32)
  deriving (Eq, Ord, Show, Generic, Model)

data Int64 = Int64 (ZigZag Word64)
  deriving (Eq, Ord, Show, Generic, Model)

-- data Integer = Integer (ZigZag (NonEmptyList Word7)) deriving (Eq, Ord, Show, Generic, Model)

data Int = Int (ZigZag Word) deriving (Eq, Ord, Show, Generic, Model)

--data String = String (Array Char) deriving (Generic,Model)

data Char = Char Word32 deriving (Eq, Ord, Show, Generic, Model)

data List a = Nil
             | Cons a (List a) deriving (Eq, Ord, Show, Generic)
instance Model a => Model (List a)

instance Model a => Model (NonEmptyList a)

-- instance Model Word7 where envType = useCT word7CT

data Unit = Unit deriving (Eq, Ord, Show, Generic, Model)
instance Model () where envType _ = envType (Proxy::Proxy Unit)

-- data Array a = Array0 | Array1 a ... | Array255 a1 .. a255 (Array a)
data Array a -- = Array [a] deriving (Eq, Ord, Show, NFData, Generic)

instance Model a => Model (Array a) where envType = useCT arrayCT

instance Model a => Model (S.Seq a) where envType _ = envType (Proxy::Proxy (Array a))

-- BUG, return as 'Array' (as these are zero-kinded, their abs type is assumed to be the same?)
-- instance AsType B.ByteString where envType _ = envType (Proxy::Proxy (Tuple2 Bool ()))
-- instance Model B.ByteString where envType _ = envType (Proxy::Proxy (Tuple2 Bool ()))
-- NOTE: need to overload AsType as well as arity of ByteString =/= Array Word8
-- instance Model B.ByteString where envType _ = nc
data Bytes = Bytes (PreAligned (Array H.Word8)) deriving (Generic,Model)

instance Model B.ByteString where envType _ = envType (Proxy::Proxy Bytes)
instance Model L.ByteString where envType _ = envType (Proxy::Proxy Bytes)
instance Model SBS.ShortByteString where envType _ = envType (Proxy::Proxy Bytes)

-- instance Model L.ByteString where envType _ = nc
-- instance Model L.ByteString where envType _ = envType (Proxy::Proxy (S.Seq H.Word8))
-- instance {-# OVERLAPPING #-} AsType (Typ L.ByteString) where asType _ = asType (undefined::Ana (S.Seq H.Word8))

-- instance Model SBS.ShortByteString where envType _ = envType (Proxy::Proxy (S.Seq H.Word8))
-- instance {-# OVERLAPPING #-} AsType (Typ SBS.ShortByteString) where asType _ = asType (undefined::Ana (S.Seq H.Word8))

--instance Model Text where envType _ = envType (Proxy::Proxy P.String)
--instance Model Text where envType _ = envType (Proxy::Proxy (BLOB UTF16LEEncoding))
-- instance {-# OVERLAPPING #-} AsType (Typ Text) where asType _ = asType (undefined::Ana (BLOB UTF16LEEncoding))

instance Model Text where envType _ = envType (Proxy::Proxy (BLOB UTF8Encoding))
instance {-# OVERLAPPING #-} AsType (Typ Text) where asType _ = asType (undefined::Ana (BLOB UTF8Encoding))

data Map a b = Map (List (a,b)) deriving Generic

instance (Model a,Model b) => Model (Map a b)

instance (Model a,Model b) => Model (M.Map a b) where envType _ = envType (Proxy::Proxy (Map a b))
-- instance {-# OVERLAPPING #-} (AsType a, AsType b) => AsType (App (App (Typ (Map A0 A1)) a) b) where asType _ = asType (undefined::(App (Typ [A0]) (App (App (Typ (A0, A1)) a) b)))

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


-- Models for types whose definition match their ideal model
instance (Model a,Model b,Model c) => Model (ADT a b c)
instance (Model a,Model b) => Model (ConTree a b)
instance Model a => Model (TypedValue a)
instance Model a => Model (ADTRef a)
instance Model a => Model (Type a)
instance Model a => Model (TypeRef a)
-- instance Model a => Model (LocalRef a)
instance Model TypedBLOB
instance Model NoEncoding
instance Model UTF8Encoding
instance Model UTF16LEEncoding
instance Model FlatEncoding
instance (Model adtName, Model consName, Model inRef, Model exRef) => Model (TypeModel adtName consName inRef exRef)
instance Model Identifier
instance Model UnicodeLetter
instance Model UnicodeLetterOrNumberOrLine
instance Model UnicodeSymbol
instance Model a => Model (SHA3_256_6 a)
instance Model AbsRef
instance Model e => Model (BLOB e)
instance Model Filler
instance Model a => Model (PreAligned a)

-- nc = error "This should have never been called!"
