module ZM.BLOB.BLOBList where

import           Data.Word

data BLOB encoding =
  BLOB
    { encoding :: encoding
    , content  :: [Word8]
    }
  deriving (Show, Read)
