{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typed.Types(
  module Data.Model.Types
  ,LocalName(..),AbsoluteType(..),ADTEnv,AbsType,AbsRef(..),AbsADT,ADTRef(..)
  -- ,ExplicitADT,ExplicitRef(..)
  ,SHA3_256_6(..)
  ,SHA3_256(..),LocalRef(..)
  ,NonEmptyList(..),nonEmptyList
  ,TypedValue(..),TypedBLOB(..),Label(..),Val(..)
  ,Timeless(..)
  ,proxyOf,Word7,Word8,Word16,Word32,Word64,Int8,Int16,Int32,Int64
  ,Identifier(..),UnicodeLetter(..),UnicodeLetterOrNumberOrLine(..),UnicodeSymbol(..)
  ) where

import           Control.DeepSeq
import           Data.Flat
import qualified Data.Map         as M
import           Data.Model.Types
import           Data.Text        (Text)
import           Data.Word
import Data.Int
import Data.Char
import qualified Data.ListLike.String as L
import Data.Foldable(toList)

instance NFData a => NFData (Array a)
instance NFData Filler
instance NFData a => NFData (PreAligned a)
instance NFData a => NFData (BLOB a)
instance NFData FlatEncoding

-- An optionally labelled value
data Label a l = Label a (Maybe l) deriving (Eq, Ord, Show, NFData, Generic)

newtype LocalName = LocalName Identifier deriving (Eq, Ord, Show, NFData, Generic)

data TypedBLOB = TypedBLOB AbsType (BLOB FlatEncoding) deriving (Eq,Ord,Show,NFData,Generic)

data TypedValue a = TypedValue AbsType a deriving (Eq, Ord, Show, Functor,NFData,  Generic)

-- data CanonicalType = CanonicalType {canonicalEnv::ADTEnv,canonicalType::AbsType}
-- ExplicitType
data AbsoluteType = AbsoluteType {canonicalEnv::ADTEnv,canonicalType::AbsType}  deriving (Eq, Ord, Show, NFData, Generic)

type ADTEnv = M.Map AbsRef AbsADT

-- Value serialised in a timeless file
data Timeless = Timeless {
  timelessMeta  :: TypedBLOB
  ,timelessValue :: BLOB FlatEncoding
  } deriving (Eq, Ord, Show,NFData,  Generic)

-- type ExplicitType = Type ExplicitRef
-- data ExplicitRef = ExplicitRef (LocalRef ExplicitADT) deriving (Eq, Ord, Show, NFData, Generic)
-- -- type ExplicitRef = (LocalRef ExplicitADT)
-- type ExplicitADT = ADT Name (ADTRef ExplicitRef)

-- Example:
data AbsRef = AbsRef (SHA3_256_6 AbsADT)  deriving (Eq, Ord,  Show, NFData, Generic)

-- type AbsADT = ADT Name Name (ADTRef AbsRef)
type AbsADT = ADT Identifier Identifier (ADTRef AbsRef)

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
--   deriving (Eq,Ord,Show,NFData,Generic)

-- Should have a variable to indicate what they point to
data Shake128_4 = Shake128_4 Word8 Word8 Word8 Word8 deriving (Eq,Ord,Show,NFData,Generic)

-- First 6 bytes of keccak 256 hash
-- data Keccak256_6 = Keccak256_6 Word8 Word8 Word8 Word8 Word8 Word8 deriving (Eq,Ord,Show,NFData,Generic)

-- data SHA3_224 = SHA3_224 (NonEmptyList Word8) deriving (Eq,Ord,Show,NFData,Generic)

data SHA3_256 a = SHA3_256 (NonEmptyList Word8) deriving (Eq,Ord,Show,NFData,Generic)

-- First 4 bytes of SHA3-256 hash
-- data SHA3_256_4 = SHA3_256_4 Word8 Word8 Word8 Word8 deriving (Eq,Ord,Show,NFData,Generic)

-- First 6 bytes of SHA3-256 hash
data SHA3_256_6 a = SHA3_256_6 Word8 Word8 Word8 Word8 Word8 Word8 deriving (Eq,Ord,Show,NFData,Generic)

-- data Shake128 = Shake128 (NonEmptyList Word8) deriving (Eq,Ord,Show,NFData,Generic)

-- Generic value (used for dynamic decoding)
data Val = Val {valType::AbsType -- Type
               ,valName::String   -- Constructor name (duplicate info if we have abstype)
               ,valBits::[Bool] -- Bit encoding/constructor id
                 -- TODO: add field names (same info in abstype)
               ,valFields::[Val]  -- Values to which the constructor is applied, if any
               } deriving  (Eq,Ord,Show,NFData, Generic)


data Word7 = Word7 Word8 deriving (Eq, Ord, Show, Generic)

-- A parsed value
data ParsedVal = ParsedVal [Bool] [ParsedVal]

type Value = TypedValue ParsedVal

proxyOf :: a -> Proxy a
proxyOf _ = Proxy ::Proxy a

-- Is it necessary to specify a syntax for identifiers?
data Identifier = Name UnicodeLetter [UnicodeLetterOrNumberOrLine]
                | Symbol (NonEmptyList UnicodeSymbol)
                deriving (Eq, Ord, Show, NFData, Generic)

instance L.StringLike Identifier where
  fromString = identifier
  toString (Name (UnicodeLetter h) t) = h : map (\(UnicodeLetterOrNumberOrLine s) -> s) t
  toString (Symbol l) = map (\(UnicodeSymbol s) -> s) . toList $ l

i = map identifier ["Tuple2","abc","<>"]

identifier :: String -> Identifier
identifier [] = error "identifier cannot be empty"
identifier s@(h:t) = if isLetter h
                     then Name (asLetter h) (map asLetterOrNumber t)
                     else Symbol (nonEmptyList $ map asSymbol s)

asSymbol :: Char -> UnicodeSymbol
asSymbol c | isSymbol c = UnicodeSymbol c
           | otherwise = error . unwords $ [show c,"is not an Unicode Symbol"]

asLetter :: Char -> UnicodeLetter
asLetter c | isLetter c = UnicodeLetter c
           | otherwise = error . unwords $ [show c,"is not an Unicode Letter"]

asLetterOrNumber :: Char -> UnicodeLetterOrNumberOrLine
asLetterOrNumber c | isLetter c || isNumber c || isAlsoOK c = UnicodeLetterOrNumberOrLine c
                   | otherwise = error . unwords $ [show c,"is not an Unicode Letter or Number"]


-- CHECK IF NEEDED
isAlsoOK '_' = True
isAlsoOK _ = False

data UnicodeLetterOrNumberOrLine = UnicodeLetterOrNumberOrLine Char deriving (Eq, Ord, Show, NFData, Generic)

{-
A character that is included in one of the following Unicode classes:
UppercaseLetter
LowercaseLetter
TitlecaseLetter
ModifierLetter
OtherLetter
-}
data UnicodeLetter = UnicodeLetter Char deriving (Eq, Ord, Show, NFData, Generic)

{-
A character that is included in one of the following Unicode classes:
DecimalNumber
LetterNumber
OtherNumber
-}
-- data UnicodeNumber = UnicodeNumber Char deriving (Eq, Ord, Show, NFData, Generic)

{-
A character that is included in one of the following Unicode classes:
MathSymbol
CurrencySymbol
ModifierSymbol
OtherSymbol
-}
data UnicodeSymbol = UnicodeSymbol Char deriving (Eq, Ord, Show, NFData, Generic)

data NonEmptyList a = Elem a
                    | Cons a (NonEmptyList a)
                    deriving (Eq,Ord,Show,NFData ,Generic,Functor,Foldable,Traversable)

nonEmptyList :: [a] -> NonEmptyList a
nonEmptyList [] = error "Cannot convert an empty list to NonEmptyList"
nonEmptyList (h:[]) = Elem h
nonEmptyList (h:t) = Cons h (nonEmptyList t)
