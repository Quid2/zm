{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Binary Large OBjects (BLOBs)
module ZM.Type.Text
  ( UTF8Text (..),
    UTF16Text (..),
  )
where

import Control.DeepSeq
import Data.Model
import Flat (Flat)
-- import ZM.Type.Array
import ZM.Type.BLOB

-- | An UTF8 encoded text
newtype UTF8Text = UTF8Text (BLOB UTF8Encoding)
  deriving (Eq, Show, NFData, Generic, Flat, Model)

-- | An UTF16 encoded text
newtype UTF16Text = UTF16Text (BLOB UTF16LEEncoding)
  deriving (Eq, Show, NFData, Generic, Flat, Model)
