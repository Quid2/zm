{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- |Binary Large OBjects (BLOBs)
module ZM.Type.BLOB
  ( BLOB(..)
  , UTF8Encoding(..)
  , UTF16LEEncoding(..)
  , FlatEncoding(..)
  , NoEncoding(..)
  )
where

import           Control.DeepSeq
import           Flat
import           Data.Model
import           ZM.Type.Array

-- |A BLOB is binary value encoded according to a specified encoding (e.g. UTF8)
data BLOB encoding =
  BLOB
    { encoding :: encoding
    , content  :: Bytes
    }
  deriving (Generic)

instance Model encoding => Model (BLOB encoding)

-- |UTF-8 Encoding
data UTF8Encoding =
  UTF8Encoding
  deriving (Eq, Ord, Show, Read, NFData, Generic, Flat, Model)

-- |UTF-16 Little Endian Encoding
data UTF16LEEncoding =
  UTF16LEEncoding
  deriving (Eq, Ord, Show, Read, NFData, Generic, Flat, Model)

-- |Flat encoding
data FlatEncoding =
  FlatEncoding
  deriving (Eq, Ord, Show, Read, NFData, Generic, Flat, Model)

-- |Unspecified encoding
data NoEncoding =
  NoEncoding
  deriving (Eq, Ord, Show, Read, NFData, Generic, Flat, Model)
