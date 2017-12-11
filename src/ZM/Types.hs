{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module ZM.Types

  (
    -- * Model
    module Data.Model.Types
  , AbsTypeModel
  , AbsType
  , AbsRef(..)
  , absRef
  , AbsADT
  , AbsEnv
  , ADTRef(..)
  , getADTRef
  , asIdentifier
  , Identifier(..)
  , UnicodeLetter(..)
  , UnicodeLetterOrNumberOrLine(..)
  , UnicodeSymbol(..)
  , SHA3_256_6(..)
  , SHAKE128_48(..)
  , NonEmptyList(..)
  , nonEmptyList
  , Word7
    -- * Encodings
  , FlatEncoding(..)
  , UTF8Encoding(..)
  , UTF16LEEncoding(..)
  , NoEncoding(..)
    -- * Exceptions
  , TypedDecoded
  , TypedDecodeException(..)
    -- *Other Re-exports
  , NFData()
  , Flat
  , ZigZag(..)
  , LeastSignificantFirst(..)
  , MostSignificantFirst(..)
  , Value(..)
  , Label(..)
  , label
  ) where

import           Control.DeepSeq
import           Control.Exception
import qualified Data.ByteString        as B
import           Data.Char
import           Data.Digest.Keccak
import           Data.Either.Validation
import           Data.Flat
import           Data.Foldable
import qualified Data.Map               as M
import           Data.Model             hiding (Name)
import           Data.Model.Types       hiding (Name)
import           Data.Word
import           ZM.Model               ()
import           ZM.Type.BLOB
import           ZM.Type.NonEmptyList
import           ZM.Type.Words          (LeastSignificantFirst (..),
                                         MostSignificantFirst (..), Word7,
                                         ZigZag (..))

-- |An absolute type, a type identifier that depends only on the definition of the type
type AbsType = Type AbsRef

-- |A reference to an absolute data type definition, in the form of a hash of the data type definition itself
-- data AbsRef = AbsRef (SHA3_256_6 AbsADT) deriving (Eq, Ord, Show, NFData, Generic, Flat)
data AbsRef = AbsRef (SHAKE128_48 AbsADT) deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- |Return the absolute reference of the given value
absRef :: Flat r => r -> AbsRef
absRef a = let [w1,w2,w3,w4,w5,w6] = B.unpack . shake_128 6 . flat $ a
           in AbsRef $ SHAKE128_48 w1 w2 w3 w4 w5 w6

-- absRef a = let [w1,w2,w3,w4,w5,w6] = B.unpack . sha3_256 6 . flat $ a
--            in AbsRef $ SHA3_256_6 w1 w2 w3 w4 w5 w6

-- |A hash of a value, the first 6 bytes of the value's SHA3-256 hash
data SHA3_256_6 a = SHA3_256_6 Word8 Word8 Word8 Word8 Word8 Word8
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- |A hash of a value, the first 48 bits (6 bytes) of the value's SHAKE128 hash
data SHAKE128_48 a = SHAKE128_48 Word8 Word8 Word8 Word8 Word8 Word8
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- CHECK: Same syntax for adt and constructor names
-- |An absolute data type definition, a definition that refers only to other absolute definitions
type AbsADT = ADT Identifier Identifier (ADTRef AbsRef)

-- |An absolute type model, an absolute type and its associated environment
type AbsTypeModel = TypeModel Identifier Identifier (ADTRef AbsRef) AbsRef

-- |An environments of absolute types
type AbsEnv = TypeEnv Identifier Identifier (ADTRef AbsRef) AbsRef

-- type ADTEnv = M.Map AbsRef AbsADT

-- |A reference inside an ADT to another ADT
data ADTRef r = Var Word8 -- ^Variable, standing for a type
              | Rec       -- ^Recursive reference to the ADT itself
              | Ext r     -- ^Reference to another ADT
  deriving (Eq, Ord, Show, NFData, Generic, Functor, Foldable, Traversable ,Flat)

-- |Return an external reference, if present
getADTRef :: ADTRef a -> Maybe a
getADTRef (Ext r) = Just r
getADTRef _       = Nothing

-- CHECK: Is it necessary to specify a syntax for identifiers?
-- |An Identifier, the name of an ADT
data Identifier = Name UnicodeLetter [UnicodeLetterOrNumberOrLine]
                | Symbol (NonEmptyList UnicodeSymbol)
                deriving (Eq, Ord, Show, NFData, Generic, Flat)

instance Flat [UnicodeLetterOrNumberOrLine]

instance Convertible String Identifier where
  -- safeConvert = errorsToConvertResult asIdentifier
  safeConvert = errorsToConvertResult (validationToEither . asIdentifier)

instance Convertible Identifier String where
  safeConvert (Name (UnicodeLetter h) t) = Right $ h : map (\(UnicodeLetterOrNumberOrLine s) -> s) t
  safeConvert (Symbol l) = Right $ map (\(UnicodeSymbol s) -> s) . toList $ l

{-|Validate a string as an Identifier

>>> asIdentifier ""
Failure ["identifier cannot be empty"]

>>> asIdentifier "Id_1"
Success (Name (UnicodeLetter 'I') [UnicodeLetterOrNumberOrLine 'd',UnicodeLetterOrNumberOrLine '_',UnicodeLetterOrNumberOrLine '1'])

>>> asIdentifier "a*^"
Failure ["In a*^: '*' is not an Unicode Letter or Number or a _","In a*^: '^' is not an Unicode Letter or Number or a _"]

>>> asIdentifier "<>"
Success (Symbol (Cons (UnicodeSymbol '<') (Elem (UnicodeSymbol '>'))))

-}
asIdentifier :: String -> Validation Errors Identifier
asIdentifier [] = err ["identifier cannot be empty"]

asIdentifier i@(h:t) = errsInContext i $
  if isLetter h
  then Name <$> asLetter h <*> sequenceA (map asLetterOrNumber t)
  else Symbol . nonEmptyList <$> sequenceA (map asSymbol i)

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

asSymbol :: Char -> Validation Errors UnicodeSymbol
asSymbol c | isSymbol c = ok $ UnicodeSymbol c
           | otherwise = err [show c,"is not an Unicode Symbol"]

asLetter :: Char -> Validation Errors UnicodeLetter
asLetter c | isLetter c = ok $ UnicodeLetter c
           | otherwise = err [show c,"is not an Unicode Letter"]

asLetterOrNumber :: Char -> Validation Errors UnicodeLetterOrNumberOrLine
asLetterOrNumber c | isLetter c || isNumber c || isAlsoOK c = ok $ UnicodeLetterOrNumberOrLine c
                   | otherwise = err $ [show c,"is not an Unicode Letter or Number or a _"]

ok :: a -> Validation e a
ok = Success

err :: [String] -> Validation Errors a
err = Failure . (:[]) . unwords

-- CHECK: IS '_' REALLY NEEDED?
isAlsoOK :: Char -> Bool
isAlsoOK '_' = True
isAlsoOK _   = False

-- |A generic value (used for dynamic decoding)
data Value = Value {valType::AbsType -- ^Type
                   ,valName::String  -- ^Constructor name (duplicate info if we have abstype)
                   ,valBits::[Bool]    -- ^Bit encoding/constructor id
                   -- TODO: add field names (same info present in abstype)
                   ,valFields::[Value]  -- ^Values to which the constructor is applied, if any
                   } deriving  (Eq,Ord,Show,NFData, Generic, Flat)

-- |An optionally labeled value
data Label a label = Label a (Maybe label) deriving (Eq, Ord, Show, NFData, Generic, Flat)

label :: (Functor f, Ord k) => M.Map k a -> (a -> l) -> f k -> f (Label k l)
label env f o = (\ref -> Label ref (f <$> M.lookup ref env)) <$> o

type TypedDecoded a = Either TypedDecodeException a

-- |An exception thrown if the decoding of a type value fails
data TypedDecodeException = UnknownMetaModel AbsType
                            | WrongType {expectedType::AbsType,actualType::AbsType}
                            | DecodeError DecodeException deriving (Show,Eq,Ord)

instance Exception TypedDecodeException

-- newtype LocalName = LocalName Identifier deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- Flat instances for data types in the 'model' package
instance (Flat adtName, Flat consName, Flat inRef, Flat exRef,Ord exRef) => Flat (TypeModel adtName consName inRef exRef)
instance (Flat a,Flat b,Flat c) => Flat (ADT a b c)
instance (Flat a,Flat b) => Flat (ConTree a b)
instance (Flat a,Flat b) => Flat [(a,Type b)]
instance Flat a => Flat [Type a]
instance Flat a => Flat (Type a)
instance Flat a => Flat (TypeRef a)

-- Model instances
instance (Model a,Model b,Model c) => Model (ADT a b c)
instance (Model a,Model b) => Model (ConTree a b)
instance Model a => Model (ADTRef a)
instance Model a => Model (Type a)
instance Model a => Model (TypeRef a)
instance (Model adtName, Model consName, Model inRef, Model exRef) => Model (TypeModel adtName consName inRef exRef)
instance Model Identifier
instance Model UnicodeLetter
instance Model UnicodeLetterOrNumberOrLine
instance Model UnicodeSymbol
instance Model a => Model (SHA3_256_6 a)
instance Model a => Model (SHAKE128_48 a)
instance Model AbsRef
instance Model a => Model (PostAligned a)
