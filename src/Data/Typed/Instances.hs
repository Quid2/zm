{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typed.Instances(
  Unit(..),Tuple2(..),Tuple3(..),Tuple4(..),Tuple5(..),Tuple6(..),Tuple7(..),Tuple8(..),Tuple9(..)
  ) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Flat
import           Data.Int
import           Data.Map             (Map)
import           Data.Model
import           Data.Text            (Text)
import qualified Data.Typed.PrimTypes as P
import           Data.Typed.Types
import           Data.Word
import           Debug.Trace
import           Type.Analyse

instance (Model a,Model b,Model c) => Model (ADT a b c)
instance (Model a,Model b) => Model (ConTree a b)
--instance Model SHA3_256_6
instance Model a => Model (ADTRef a)
instance Model a => Model (Type a)
instance Model a => Model (TypeRef a)
instance Model a => Model (LocalRef a)
--instance Model ExplicitRef
instance Model Timeless
instance Model TypedBLOB
instance Model UTF8Encoding
instance Model FlatEncoding
instance Model AbsoluteType
instance Model Identifier
instance Model UnicodeLetter
instance Model UnicodeLetterOrNumberOrLine
instance Model UnicodeSymbol

instance Model a => Model (SHA3_256_6 a)
instance Model AbsRef

instance Model Filler
instance Model a => Model (PreAligned a)

nc = error "This should have never been called!"

-- BUG, return as 'Array' (as these are zero-kinded, their abs type is assumed to be the same?)
-- instance AsType B.ByteString where envType _ = envType (Proxy::Proxy (Tuple2 Bool ()))
-- instance Model B.ByteString where envType _ = envType (Proxy::Proxy (Tuple2 Bool ()))
-- NOTE: need to overload AsType as well as arity of ByteString =/= Array Word8
instance Model B.ByteString where envType _ = nc
-- instance Model B.ByteString where envType _ = envType (Proxy::Proxy (Array Word8))
instance {-# OVERLAPPING #-} AsType (Typ B.ByteString) where asType _ = asType (undefined::Ana (Array Word8))

instance Model L.ByteString where envType _ = nc
-- instance Model L.ByteString where envType _ = envType (Proxy::Proxy (Array Word8))
instance {-# OVERLAPPING #-} AsType (Typ L.ByteString) where asType _ = asType (undefined::Ana (Array Word8))

--instance Model Text where envType _ = envType (Proxy::Proxy P.String)
-- instance Model Text where envType _ = envType (Proxy::Proxy (BLOB UTF8Encoding))
instance Model Text where envType _ = nc
instance {-# OVERLAPPING #-} AsType (Typ Text) where asType _ = asType (undefined::Ana (BLOB UTF8Encoding))

instance Model e => Model (BLOB e)

instance (Model a,Model b) => Model (Map a b) where envType _ = nc
instance {-# OVERLAPPING #-} (AsType a, AsType b) => AsType (App (App (Typ (Map A0 A1)) a) b) where asType _ = asType (undefined::(App (Typ [A0]) (App (App (Typ (A0, A1)) a) b)))

data Unit = Unit deriving (Eq, Ord, Show, Generic, Model)
instance Model () where envType _ = envType (Proxy::Proxy Unit)

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

instance Flat a => Flat (SHA3_256_6 a)
instance Flat AbsRef
instance Flat AbsoluteType
instance Flat Identifier
instance Flat UnicodeLetter
instance Flat UnicodeLetterOrNumberOrLine
instance Flat UnicodeSymbol

instance (Flat a,Flat b,Flat c) => Flat (ADT a b c)
instance (Flat a,Flat b) => Flat (ConTree a b)
-- instance Flat SHA3_256_6
instance Flat a => Flat (ADTRef a)
instance Flat a => Flat (Type a)
instance Flat a => Flat (TypeRef a)
instance Flat a => Flat (NonEmptyList a)
instance Flat Timeless
instance Flat TypedBLOB
-- instance Model TypedBLOB
instance Flat a => Flat (TypedValue a)
instance Model a => Model (TypedValue a)
-- instance Model Bytes
