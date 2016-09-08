{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typed.Types(
  module Data.Model.Types
  ,LocalName(..),AbsoluteType(..),ADTEnv,AbsType,AbsRef,AbsADT,ADTRef(..)
  --,Ref(..)
  --,Shake128_4(..),Keccak256_6(..)
  ,SHA3_256_6(..),LocalRef(..)
  ,NonEmptyList(..),nonEmptyList
  ,TypedValue(..),TypedBytes(..),Label(..),Val(..)
  ,Timeless(..)
  ,proxyOf,Word7(..)
  ) where

import           Control.DeepSeq
import           Data.Flat
import qualified Data.Map         as M
import           Data.Model.Types
import           Data.Text        (Text)
import           Data.Word
import Data.Int

instance NFData a => NFData (Array a)
instance NFData Filler
instance NFData a => NFData (PreAligned a)
instance NFData a => NFData (BLOB a)
instance NFData FlatEncoding

data Word7 = Word7 Word8 deriving (Eq, Ord, Show, Generic)

data Label a = Label a (Maybe Text) deriving (Eq, Ord, Show, NFData, Generic)

newtype LocalName = LocalName Text deriving (Eq, Ord, Show, NFData, Generic)

data TypedBytes = TypedBytes AbsType (BLOB FlatEncoding) deriving (Eq,Ord,Show,NFData,Generic)

data TypedValue a = TypedValue AbsType a deriving (Eq, Ord, Show, Functor,NFData,  Generic)

-- data CanonicalType = CanonicalType {canonicalEnv::ADTEnv,canonicalType::AbsType}
data AbsoluteType = AbsoluteType {canonicalEnv::ADTEnv,canonicalType::AbsType}  deriving (Eq, Ord, Show, NFData, Generic)

type ADTEnv = M.Map AbsRef AbsADT

-- Value serialised in a timeless file
data Timeless = Timeless {
  --timelessMeta :: ExplicitType -- Type of adt
  --,timelessType  :: BLOB FlatEncoding
  timelessMeta  :: TypedBytes -- TypedValue (Typed)
  --,timelessType  :: Type r -- BLOB FlatEncoding
  ,timelessValue :: BLOB FlatEncoding
  } deriving (Eq, Ord, Show,NFData,  Generic)

-- data CanonicalADT = CanonicalADT (ADT String (ADTRef ))
-- Is it necessarily saturated?
-- data SaturatedADT = SaturatedADT (ADT String )
-- data SaturatedRef r = RecRef | ExtRef (SaturatedADT)

type ExplicitType = Type ExplicitRef

data ExplicitRef = ExplicitRef (LocalRef ExplicitADT) -- SHA3_224_6 -- AbsADT -- Ref AbsADT

type ExplicitADT = ADT Text (ADTRef ExplicitRef)

-- Example: 
-- type AbsRef = Ref AbsADT
-- type AbsRef = SHA3_256 AbsADT -- Ref AbsADT
type AbsRef = SHA3_256_6 -- Keccak256_6 -- AbsADT -- Ref AbsADT

-- type AbsADT = ADT String (ADTRef (SHA3_256 AbsADT))
-- type AbsADT = ADT String (ADTRef SHA3_224)
type AbsADT = ADT
  Text -- String might be shorter
  (ADTRef AbsRef)

-- Absolute Type
type AbsType = Type AbsRef

-- Internal reference, into an ADT, to another ADT.
data ADTRef r =
    Var Word8  -- Variable
    | Rec      -- Recursive reference
    -- | Ext AbsRef  -- Pointer to an external adt
    | Ext r    -- AbsRef  -- Pointer to an external adt
    deriving (Eq, Ord, Show, NFData, Generic)

-- A local reference, used to embed multiple times the same value.
data LocalRef a = LocalDef a      -- Definition -- Word a Word is not necessary if we have a canonical order of traversal
                | LocalRef Word   -- Reference to definition, 0-based position, head-first, in a list containing all distinct values in structure, added in depth-first left-first order.
    deriving (Eq, Ord, Show, NFData, Generic)

-- data Ref a =
--   Verbatim (NonEmptyList Word8) -- NO: must be explicitly padded. White padded serialisation (if required, exact bits can be recovered by decoding and recoding.. or byte padding has to be part of the definition!)
--   -- | List Bit -- Express exactly the right number of bits (1/16 bits overhead per bit), useful property: adding serialised sequences without the need of decoding them.
--   | Shake128 (NonEmptyList Word8)
--   -- | Hash Shake128  -- A, possibly infinite sequence of bytes (useful up to 256 bit), shorter codes are prefixes of longer ones.
--   deriving (Eq,Ord,Read,Show,NFData,Generic)

-- Should have a variable to indicate what they point to
data Shake128_4 = Shake128_4 Word8 Word8 Word8 Word8 deriving (Eq,Ord,Read,Show,NFData,Generic)

-- First 6 bytes of keccak 256 hash
-- data Keccak256_6 = Keccak256_6 Word8 Word8 Word8 Word8 Word8 Word8 deriving (Eq,Ord,Read,Show,NFData,Generic)

data SHA3_224 = SHA3_224 (NonEmptyList Word8) deriving (Eq,Ord,Read,Show,NFData,Generic)

data SHA3_256 a = SHA3_256 (NonEmptyList Word8) deriving (Eq,Ord,Read,Show,NFData,Generic)

-- First 4 bytes of SHA3-256 hash
-- data SHA3_256_4 = SHA3_256_4 Word8 Word8 Word8 Word8 deriving (Eq,Ord,Read,Show,NFData,Generic)

-- First 6 bytes of SHA3-256 hash
data SHA3_256_6 = SHA3_256_6 Word8 Word8 Word8 Word8 Word8 Word8 deriving (Eq,Ord,Read,Show,NFData,Generic)

-- data Shake128 = Shake128 (NonEmptyList Word8) deriving (Eq,Ord,Read,Show,NFData,Generic)

data NonEmptyList a = Elem a
                    | Cons a (NonEmptyList a)
                    deriving (Eq,Ord,Show,Read,NFData ,Generic,Functor,Foldable,Traversable)

nonEmptyList :: [a] -> NonEmptyList a
nonEmptyList [] = error "Cannot convert an empty list to NonEmptyList"
nonEmptyList (h:[]) = Elem h
nonEmptyList (h:t) = Cons h (nonEmptyList t)

-- Generic value (used for dynamic decoding)
data Val = Val {valType::AbsType -- Type
               ,valName::Text   -- Constructor name (duplicate info if we have abstype)
               ,valBits::[Bool] -- Bit encoding/constructor id
                 -- TODO: add field names (same info in abstype)
               ,valFields::[Val]  -- Values to which the constructor is applied, if any
               } deriving  (Eq,Ord,Read,Show,NFData, Generic)

-- A parsed value
data ParsedVal = ParsedVal [Bool] [ParsedVal]

type Value = TypedValue ParsedVal

proxyOf :: a -> Proxy a
proxyOf _ = Proxy ::Proxy a

