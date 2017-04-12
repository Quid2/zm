{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.BLOB (BLOB(..),BLOBLike(..),FlatEncoding(..),UTF8Encoding(..),UTF16LEEncoding(..),NoEncoding(..))where

import           Control.DeepSeq
import qualified Data.ByteString     as B
import qualified Data.ByteString.Lazy as L
import           Data.Flat.Class
import           Data.Flat.Instances ()
import           Data.Word

data UTF8Encoding = UTF8Encoding
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

data UTF16LEEncoding = UTF16LEEncoding
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

data NoEncoding = NoEncoding deriving (Eq, Ord, Show, NFData, Generic, Flat)

data FlatEncoding = FlatEncoding deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- The encoding is embedded as a value in order to support encodings that might have multiple values/variations.
data BLOB encoding = BLOB encoding B.ByteString
  deriving (Eq, Ord, Show, NFData, Generic, Flat)

-- blob :: encoding -> B.ByteString -> BLOB encoding
--blob enc = BLOB enc . preAligned
-- blob = BLOB

-- blobBytes :: encoding -> [Word8] -> BLOB encoding
-- blobBytes enc = blob enc . B.pack

-- unblob :: BLOB encoding -> B.ByteString
-- unblob (BLOB _ pa) = preValue pa
-- unblob (BLOB _ pa) = pa

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
