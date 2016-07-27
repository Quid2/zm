-- Imports for generated Data Types files
{-# LANGUAGE NoMonomorphismRestriction #-}
module Quid2.QQ.Pattern(run,Get,decode
                       ,Bool(True)--,(<$>)
                       ,match8,match16,match32,match64,skip
                       ,ByteString) where
import Control.Applicative
import Data.Binary.Bits.Get
import Data.Flat
import Data.ByteString.Lazy

-- TODO: check there is no left over data?
run = runGet . fmap and . sequence

skip = (const True <$>)

match8 n v  = (== v) <$> getWord8 n
match16 n v = (== v) <$> getWord16be n
match32 n v = (== v) <$> getWord32be n
match64 n v = (== v) <$> getWord64be n

