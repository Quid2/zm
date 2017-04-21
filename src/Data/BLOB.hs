{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
-- Binary Large OBjects (BLOBs)
module Data.BLOB (
    BLOB(..),
    BLOBLike(..),
    -- *Encodings
    FlatEncoding(..),
    UTF8Encoding(..),
    UTF16LEEncoding(..),
    NoEncoding(..)
    ) where

import           Control.DeepSeq
import qualified Data.ByteString     as B
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Class
import           Data.Flat.Instances ()
import           Data.Word

-- UTF-8 Little Endian Encoding
data UTF8Encoding = UTF8Encoding
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- UTF-16 Little Endian Encoding
data UTF16LEEncoding = UTF16LEEncoding
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- <http://quid2.org/ Flat> encoding
data FlatEncoding = FlatEncoding deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- Unspecified encoding
data NoEncoding = NoEncoding deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- A BLOB is composed by a binary encoding and an encoded binary value
data BLOB encoding = BLOB encoding B.ByteString
  deriving (Eq, Ord, NFData, Generic, Flat, Show)

-- instance Show encoding => Show (BLOB encoding) where show (BLOB enc bs) = unwords ["BLOB",show enc,show $ B.unpack bs]

-- blob :: encoding -> B.ByteString -> BLOB encoding
--blob enc = BLOB enc . preAligned
-- blob = BLOB

-- blobBytes :: encoding -> [Word8] -> BLOB encoding
-- blobBytes enc = blob enc . B.pack

-- unblob :: BLOB encoding -> B.ByteString
-- unblob (BLOB _ pa) = preValue pa
-- unblob (BLOB _ pa) = pa

-- Utility class used to construct/deconstruct BLOBs from/to different representations of binary data
class BLOBLike bs where
  blob :: encoding -> bs -> BLOB encoding
  unblob :: BLOB encoding -> bs

instance BLOBLike [Word8] where
  blob enc = BLOB enc . B.pack
  unblob (BLOB _ pa) = B.unpack pa

instance BLOBLike B.ByteString where
  blob = BLOB
  unblob (BLOB _ pa) = pa

instance BLOBLike L.ByteString where
  blob enc = BLOB enc . L.toStrict
  unblob (BLOB _ pa) = L.fromStrict pa
