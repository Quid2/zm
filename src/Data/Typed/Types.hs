{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typed.Types(
  module Data.Model.Types
  ,LocalName(..),AbsoluteType(..),ADTEnv,AbsEnv,AbsType,AbsRef,AbsADT,RelADT,ADTRef(..)
  ,MutualAbsType,MutualAbsRef,MutualADTEnv,MutualAbsADT,MutualADTRef(..),Ref(..)
  ,NonEmptyList(..),nonEmptyList
  ,TypedValue(..),TypedBytes(..),Label(..),Val(..)
  ,proxyOf
  ) where

import           Control.DeepSeq
import           Data.Flat
import qualified Data.Map         as M
import           Data.Model.Types
import           Data.Text        (Text)
import           Data.Word

data Label a = Label a (Maybe Text) deriving (Eq, Ord, Show, NFData, Generic)

newtype LocalName = LocalName Text deriving (Eq, Ord, Show, NFData, Generic)

-- data TypedBytes = TypedBytes AbsType Bytes deriving (Eq, Ord, Show,NFData,  Generic)

data TypedBytes = TypedBytes AbsType (BLOB FlatEncoding) deriving (Eq, Ord, Show,NFData,  Generic)

instance NFData a => NFData (Array a)
instance NFData Filler
instance NFData a => NFData (PreAligned a)
instance NFData a => NFData (BLOB a)
instance NFData FlatEncoding

data TypedValue a = TypedValue AbsType a deriving (Eq, Ord, Show, Functor,NFData,  Generic)

-- data CanonicalType = CanonicalType {canonicalEnv::ADTEnv,canonicalType::AbsType}
data AbsoluteType = AbsoluteType {canonicalEnv::ADTEnv,canonicalType::AbsType}  deriving (Eq, Ord, Show, NFData, Generic)

type ADTEnv = M.Map AbsRef AbsADT

-- Absolute Type
type AbsType = Type AbsRef

type AbsRef = Ref AbsADT

data ADTRef =
    Var Word8     -- Variable
    | Rec         -- Recursive reference
    | Ext AbsRef  -- Pointer to an external adt
    deriving (Eq, Ord, Show, NFData, Generic)

-- Mutual recursive model
-- BUG: Possible name clash.
type AbsEnv = M.Map Text (MutualAbsRef,MutualAbsADT)

type MutualADTEnv = M.Map MutualAbsRef MutualAbsADT

type MutualAbsType = Type MutualAbsRef

type MutualAbsRef = Ref MutualAbsADT

{-
Canonical ADT, requirements:
a) unambiguous
b) support for recursive definitions
  or not.

Algorithm:
1) Split ADTs in mutually recursive sets
2) For every set, for every adt in the set its canonical representation is as a list of the ADTs in the recursive set, starting with the adt being represented and the other ordered by name ? with links to recursive adts represented as strings.

Or:
A single ADT with
-}

type AbsADT = ADT Text ADTRef

type MutualAbsADT = NonEmptyList RelADT

type RelADT = ADT Text MutualADTRef

data MutualADTRef =
   MVar Word8     -- Variable
   | MRec Text  -- Recursive reference, either to the type being defined or a mutually recursive type
   | MExt MutualAbsRef -- Pointer to external definition
   deriving (Eq, Ord, Show, NFData, Generic)

data Ref a =
  Verbatim (NonEmptyList Word8) -- NO: must be explicitly padded. White padded serialisation (if required, exact bits can be recovered by decoding and recoding.. or byte padding has to be part of the definition!)
  -- | List Bit -- Express exactly the right number of bits (1/16 bits overhead per bit), useful property: adding serialised sequences without the need of decoding them.
  | Shake128 (NonEmptyList Word8)
  -- | Hash Shake128  -- A, possibly infinite sequence of bytes (useful up to 256 bit), shorter codes are prefixes of longer ones.
  deriving (Eq,Ord,Read,Show,NFData, Generic)

-- data Shake128 = Shake128 (NonEmptyList Word8) deriving (Eq,Ord,Read,Show,Generic)

data NonEmptyList a = Elem a
                    | Cons a (NonEmptyList a)
                    deriving (Eq,Ord,Show,Read,NFData ,Generic,Functor,Foldable,Traversable)

nonEmptyList :: [a] -> NonEmptyList a
nonEmptyList [] = error "Cannot convert an empty list to NonEmptyList"
nonEmptyList (h:[]) = Elem h
nonEmptyList (h:t) = Cons h (nonEmptyList t)

-- Generic value (used for dynamic decoding)
-- TODO: add type
data Val =
--Val
--     Text   -- Constructor name
--     [Bool] -- Bit encoding (for debugging purposes)
--     [Val]  -- Values to which the constructor is applied, if any
--     |
  TVal
    AbsType -- Type
    Text   -- Constructor name
    [Bool] -- Bit encoding (for debugging purposes)
    [Val]  -- Values to which the constructor is applied, if any
 deriving  (Eq,Ord,Read,Show,NFData, Generic)

proxyOf :: a -> (Proxy a)
proxyOf _ = Proxy ::Proxy a

