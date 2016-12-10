{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typed.Types (
    -- *Model
    module Data.Model.Types,
    TypedValue(..),
    TypedBLOB(..),
    AbsTypeModel,
    --ADTEnv,
    AbsType,
    AbsRef(..),absRef,
    AbsADT,
    AbsEnv,
    ADTRef(..),getADTRef,
    Identifier(..),
    UnicodeLetter(..),
    UnicodeLetterOrNumberOrLine(..),
    UnicodeSymbol(..),
    SHA3_256_6(..),
    --,SHA3_256(..)
    NonEmptyList(..),nonEmptyList,
    ZigZag(..),LeastSignificantFirst(..),MostSignificantFirst(..),
    -- Transform
    Label(..),label,
    -- LocalRef(..),
    --
    Word7,
    -- Word8,
    -- Word16,
    -- Word32,
    -- Word64,
    -- Int8,
    -- Int16,
    -- Int32,
    -- Int64,
    --LocalName(..)

    -- *Re-exports
    NFData(),Flat
    -- Array,
    -- Tuple2(..),Tuple3(..),Tuple4(..),Tuple5(..),Tuple6(..),Tuple7(..),Tuple8(..),Tuple9(..)
    ) where

import           Control.DeepSeq
import           Data.Flat
import qualified Data.Map         as M
import           Data.Model.Types hiding (Name)
import           Data.Word
import           Data.Char
import qualified Data.ListLike.String as L
import           Data.Foldable (toList)
import qualified Data.ByteString.Lazy as L
import           Data.Digest.SHA3
import qualified Data.ByteString      as B
import qualified Data.List.NonEmpty as NE

-- |A typed value, a flat encoded value plus its absolute type
data TypedBLOB = TypedBLOB AbsType (BLOB FlatEncoding)
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- |A typed value, a value plus its absolute type
data TypedValue a = TypedValue AbsType a deriving (Eq, Ord, Show, Functor,NFData,  Generic, Flat)

-- |An absolute type, a type identifier that depends only on the definition of the type
type AbsType = Type AbsRef

-- |A reference to an absolute data type definition, in the form of a hash of the data type definition itself
data AbsRef = AbsRef (SHA3_256_6 AbsADT)
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

absRef :: Flat r => r -> AbsRef
absRef a = let ([w1,w2,w3,w4,w5,w6]) = B.unpack . sha3_256 6 . L.toStrict . flat $ a
           in AbsRef $ SHA3_256_6 w1 w2 w3 w4 w5 w6

-- |A hash of a value, the first 6 bytes of the value's SHA3-256 hash
data SHA3_256_6 a = SHA3_256_6 Word8 Word8 Word8 Word8 Word8 Word8
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- |An absolute data type definition, a definition that refers only to other absolute definitions
-- CHECK: Same syntax for adt and constructor names
type AbsADT = ADT Identifier Identifier (ADTRef AbsRef)

type AbsTypeModel = TypeModel Identifier Identifier (ADTRef AbsRef) AbsRef

type AbsEnv = TypeEnv Identifier Identifier (ADTRef AbsRef) AbsRef

-- type ADTEnv = M.Map AbsRef AbsADT

-- |An internal reference to a , a reference into an ADT to another ADT
-- PROF: as we do not support higher kinds, a variable can only point to a Type, not an ADT
-- NOTE: why not using TypeRef ?
data ADTRef r = Var Word8 -- ^Variable
              | Rec       -- ^Recursive reference to the ADT
              | Ext r     -- ^Reference to another ADT
  -- | Ext AbsRef  -- Pointer to an external adt
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- WHAT ABOUT REC?
getADTRef (Ext r) = Just r
getADTRef _       = Nothing

-- |An Identifier, the name of an ADT
-- Is it necessary to specify a syntax for identifiers?
data Identifier = Name UnicodeLetter [UnicodeLetterOrNumberOrLine]
--                | Symbol (NE.NonEmpty UnicodeSymbol)
                | Symbol (NonEmptyList UnicodeSymbol)
                deriving (Eq, Ord, Show, NFData, Generic, Flat)

instance L.StringLike Identifier where
  fromString = identifier
  toString (Name (UnicodeLetter h) t) = h : map (\(UnicodeLetterOrNumberOrLine s) -> s) t
  toString (Symbol l) = map (\(UnicodeSymbol s) -> s) . toList $ l

-- |A character that is either a `UnicodeLetter`, a `UnicodeNumber` or the special character '_'
data UnicodeLetterOrNumberOrLine = UnicodeLetterOrNumberOrLine Char deriving (Eq, Ord, Show, NFData, Generic, Flat)

{-|
A character that is included in one of the following Unicode classes:
UppercaseLetter
LowercaseLetter
TitlecaseLetter
ModifierLetter
OtherLetter
-}
data UnicodeLetter = UnicodeLetter Char deriving (Eq, Ord, Show, NFData, Generic, Flat)

{-|
A character that is included in one of the following Unicode classes:
DecimalNumber
LetterNumber
OtherNumber
-}
data UnicodeNumber = UnicodeNumber Char deriving (Eq, Ord, Show, NFData, Generic, Flat)

{-|
A character that is included in one of the following Unicode classes:
MathSymbol
CurrencySymbol
ModifierSymbol
OtherSymbol
-}
data UnicodeSymbol = UnicodeSymbol Char deriving (Eq, Ord, Show, NFData, Generic, Flat)

i = map identifier ["Tuple2","abc","<>"]

identifier :: String -> Identifier
identifier [] = error "identifier cannot be empty"
identifier s@(h:t) = if isLetter h
                     then Name (asLetter h) (map asLetterOrNumber t)
--                     else Symbol (NE.map asSymbol s)
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

-- |A 7 bits word
data Word7 = Word7 Word8 deriving (Eq, Ord, Show, Generic, Flat)

-- |ZigZag encoding, map signed integers to unsigned integers
-- Positive integers are mapped to even unsigned values, negative integers to odd values:
-- 0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, 2 -> 4 ...
data ZigZag a = ZigZag a
  deriving (Eq, Ord, Show, Generic, Flat)

data LeastSignificantFirst a = LeastSignificantFirst a
  deriving (Eq, Ord, Show, Generic)

data MostSignificantFirst a = MostSignificantFirst a
  deriving (Eq, Ord, Show, Generic)


-- A parsed value
-- data ParsedVal = ParsedVal [Bool] [ParsedVal]

-- type Value = TypedValue ParsedVal

-- An optionally labelled value
data Label a l = Label a (Maybe l) deriving (Eq, Ord, Show, NFData, Generic, Flat)

label :: (Functor f, Ord k) => M.Map k a -> (a -> l) -> f k -> f (Label k l)
label env f o = (\ref -> Label ref (f <$> M.lookup ref env)) <$> o

-- newtype LocalName = LocalName Identifier deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- Flat instances for data types in the 'model' package
instance (Flat adtName, Flat consName, Flat inRef, Flat exRef,Ord exRef) => Flat (TypeModel adtName consName inRef exRef)
instance (Flat a,Flat b,Flat c) => Flat (ADT a b c)
instance (Flat a,Flat b) => Flat (ConTree a b)
instance Flat a => Flat (Type a)
instance Flat a => Flat (TypeRef a)

-- |A list that contains at least one element
data NonEmptyList a = Elem a
                    | Cons a (NonEmptyList a)
  deriving (Eq, Ord, Show, NFData, Generic, Functor, Foldable, Traversable, Flat)

-- |Convert a list to a `NonEmptyList`, returns an error if the list is empty
nonEmptyList :: [a] -> NonEmptyList a
nonEmptyList [] = error "Cannot convert an empty list to NonEmptyList"
nonEmptyList (h:[]) = Elem h
nonEmptyList (h:t) = Cons h (nonEmptyList t)

