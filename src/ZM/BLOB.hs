{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZM.BLOB (
  BLOB (..),
  blob,
  unblob,
  TypedBLOB (..),
  typedBLOB,
  typedBLOB_,
  untypedBLOB,
  TypedValue (..),
  typedValue,
  untypedValue,
  typeErr,
) where

import Control.DeepSeq
import Data.Bifunctor
import qualified Data.ByteString as B
import Data.ByteString.Convert
import Data.Model
import qualified Data.Text.Encoding as T
import Flat
import Text.PrettyPrint.HughesPJClass hiding (first)
import Text.Read
import ZM.Abs
import qualified ZM.BLOB.BLOBList as BL
import ZM.Model ()
import qualified ZM.Type.BLOB as Z
import ZM.Types
import ZM.Util (proxyOf)

-- | A BLOB is binary value encoded according to a specified encoding (e.g. UTF8)
data BLOB encoding = BLOB
  { encoding :: encoding
  , content :: B.ByteString
  }
  deriving (Eq, Ord, NFData, Generic, Flat)

instance (Model encoding) => Model (BLOB encoding)

{- |
\$setup
>>> import Data.Word
>>> b = blob NoEncoding [11::Word8,22,33]

>>> b
BLOB {encoding= NoEncoding ,content= [11,22,33] }

>>> read (show b) == b
True
-}
instance (Show encoding) => Show (BLOB encoding) where
  show (BLOB enc bs) =
    unwords ["BLOB {encoding=", show enc, ",content=", show $ B.unpack bs, "}"]

instance (Read encoding) => Read (BLOB encoding) where
  readPrec = (\(BL.BLOB e c) -> BLOB e (B.pack c)) <$> readPrec

-- | Extract the binary content of a BLOB
unblob :: BLOB t -> B.ByteString
unblob = content

-- | Build a BLOB from an encoding and a ByteString-like value
blob :: (AsByteString a) => encoding -> a -> BLOB encoding
blob enc = BLOB enc . toByteString

-- | A typed value, a Flat encoded value and its absolute type
data TypedBLOB
  = TypedBLOB AbsType (BLOB Z.FlatEncoding)
  deriving (Eq, Ord, Show, NFData, Generic, Flat, Model)

{- | Build a TypedBLOB out of a value

>>> typedBLOB True
TypedBLOB (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))) BLOB {encoding= FlatEncoding ,content= [129] }
-}
typedBLOB :: forall a. (Model a, Flat a) => a -> TypedBLOB
typedBLOB = typedBLOB_ (absType (Proxy :: Proxy a))

-- | Build a TypedBLOB out of a type and a value
typedBLOB_ :: (Flat a) => AbsType -> a -> TypedBLOB
typedBLOB_ t v = TypedBLOB t (blob Z.FlatEncoding . flat $ v)

-- | A typed value, a value and its absolute type
data TypedValue a
  = TypedValue AbsType a
  deriving (Eq, Ord, Show, Functor, NFData, Generic, Flat)

-- | Build a TypedValue out of a value
typedValue :: forall a. (Model a) => a -> TypedValue a
typedValue = TypedValue (absType (Proxy :: Proxy a))

-- | Type-checked extraction of a value of a known type from a decoded TypedBLOB
untypedBLOB ::
  forall a. (Flat a, Model a) => Decoded TypedBLOB -> TypedDecoded a
untypedBLOB ea = case ea of
  Left e -> Left . DecodeError $ e
  Right (TypedBLOB typ' bs) ->
    let typ = absType (Proxy :: Proxy a)
     in if typ' /= typ
          then typeErr typ typ'
          else first DecodeError . unflat $ (unblob bs :: B.ByteString)

-- | Type-checked extraction of a value of a known type from a decoded TypedValue
untypedValue :: (Model a) => Decoded (TypedValue a) -> TypedDecoded a
untypedValue ea = case ea of
  Left e -> Left . DecodeError $ e
  Right (TypedValue typ' a) ->
    let typ = absType (proxyOf a)
     in if typ' /= typ then typeErr typ typ' else Right a

-- | Return a WrongType error
typeErr :: AbsType -> AbsType -> TypedDecoded a
typeErr typ typ' = Left $ WrongType typ typ'

instance (Show a) => Pretty (TypedValue a) where
  pPrint (TypedValue t v) = text (show v) <+> text "::" <+> pPrint t

instance (Pretty encoding) => Pretty (BLOB encoding) where
  pPrint (BLOB enc bs) = text "BLOB" <+> pPrint enc <+> pPrint bs

instance {-# OVERLAPS #-} Pretty (BLOB UTF8Encoding) where
  pPrint :: BLOB UTF8Encoding -> Doc
  pPrint = pPrint . T.decodeUtf8 . unblob

instance {-# OVERLAPS #-} Pretty (BLOB UTF16LEEncoding) where
  pPrint = pPrint . T.decodeUtf16LE . unblob
