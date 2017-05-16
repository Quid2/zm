{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Data.Typed.BLOB (
    BLOB(..),
    blob,
    unblob,
    TypedBLOB(..),
    typedBLOB,
    typedBLOB_,
    untypedBLOB,
    TypedValue(..),
    typedValue,
    untypedValue,
    typeErr,
    ) where

import           Control.DeepSeq
import           Data.Bifunctor
import qualified Data.ByteString         as B
import           Data.ByteString.Convert
import           Data.Flat
import           Data.Model
import           Data.Typed.Abs
import           Data.Typed.Model        ()
import qualified Data.Typed.Type.BLOB    as Z
import           Data.Typed.Types
import           Data.Typed.Util

-- |A BLOB is binary value encoded according to a specified encoding (e.g. UTF8)
data BLOB encoding = BLOB {encoding::encoding,content::B.ByteString}
  deriving (Eq, Ord, NFData, Generic, Flat)

instance Model encoding => Model (BLOB encoding)

instance Show encoding => Show (BLOB encoding) where show (BLOB enc bs) = unwords ["BLOB",show enc,show $ B.unpack bs]

-- |Extract the binary content of a BLOB
unblob :: BLOB t -> B.ByteString
--unblob (BLOB _ bs) = bs
unblob = content

-- |Build a BLOB from an encoding and a ByteString-like value
blob :: AsByteString a => encoding -> a -> BLOB encoding
blob enc = BLOB enc . toByteString

-- |A typed value, a Flat encoded value and its absolute type
data TypedBLOB = TypedBLOB AbsType (BLOB Z.FlatEncoding)
  deriving (Eq, Ord, Show, NFData, Generic, Flat, Model)

-- |Build a TypedBLOB out of a value
typedBLOB :: forall a . (Model a,Flat a) => a -> TypedBLOB
typedBLOB = typedBLOB_ (absType (Proxy :: Proxy a))

-- |Build a TypedBLOB out of a type and a value
typedBLOB_ :: Flat a => AbsType -> a -> TypedBLOB
typedBLOB_ t v = TypedBLOB t (blob Z.FlatEncoding . flat $ v)

-- |A typed value, a value and its absolute type
data TypedValue a = TypedValue AbsType a deriving (Eq, Ord, Show, Functor,NFData,  Generic, Flat)

-- |Build a TypedValue out of a value
typedValue :: forall a . Model a => a -> TypedValue a
typedValue = TypedValue (absType (Proxy :: Proxy a))

-- |Type-checked extraction of a value of a known type from a decoded TypedBLOB
untypedBLOB ::  forall a.  (Flat a, Model a) => Decoded TypedBLOB -> TypedDecoded a
untypedBLOB ea = case ea of
                    Left e -> Left . DecodeError $ e
                    Right (TypedBLOB typ' bs) ->
                      let typ = absType (Proxy :: Proxy a)
                      in if typ' /= typ
                         then typeErr typ typ'
                         else first DecodeError . unflat $ (unblob bs :: B.ByteString)

-- |Type-checked extraction of a value of a known type from a decoded TypedValue
untypedValue ::  Model a => Decoded (TypedValue a) -> TypedDecoded a
untypedValue ea = case ea of
                    Left e -> Left . DecodeError $ e
                    Right (TypedValue typ' a) ->
                      let typ = absType (proxyOf a)
                      in if typ' /= typ
                         then typeErr typ typ'
                         else Right a

-- |Return a WrongType error
typeErr :: AbsType -> AbsType -> TypedDecoded a
typeErr typ typ' = Left $ WrongType typ typ'
