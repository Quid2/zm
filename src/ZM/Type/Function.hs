{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module ZM.Type.Function where

import           Data.Flat
import           Data.Model
import           ZM.Types   (SHAKE128_48)

-- Here we would need higher order types:
-- data Function f k r = Call f | Reply (k f) r deriving (Eq,Ord,Show,Generic,Flat)
-- instance (Model a,Model (b a),Model c) => Model (Function a b c)
-- data Function f k r = Call f | Reply k r deriving (Eq,Ord,Show,Generic,Flat)
-- instance (Model a,Model b,Model c) => Model (Function a b c)
-- Or:
data Function f r
  = Call f
  | Reply (SHAKE128_48 f) r
  deriving (Eq, Ord, Show, Generic, Flat)

instance (Model a, Model b) => Model (Function a b)
