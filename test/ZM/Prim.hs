{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric ,CPP  #-}

module ZM.Prim(Z.Word7,H.Word8,Z.Array(..)) where
import           Flat
import           Flat.Decoder
import           Flat.Encoder
-- import           Data.Model
import qualified Data.Word        as H
-- import           ZM.Type.Generate
import qualified ZM.Type.Array as Z
import qualified ZM.Type.Words as Z
import ZM.Model()

-- type Word7 = H.Word8


-- -- |A 7 bits unsigned integer
-- -- data Word7 = V0 .. V127
-- data Word7 = Word7 H.Word8 deriving (Eq, Ord, Show, Generic)
-- instance Model Word7 where envType = useCT word7CT



-- TO BE CHECKED
instance Flat Z.Word7 where
  encode (Z.Word7 w) = eBits 7 w
  decode  = Z.Word7 <$> dBEBits8 7
  size _ n = n+7

-- -- |An 8 bits unsigned integer
-- -- data Word8 = V0 | V1 .. | V255
-- data Word8 = Word8 H.Word8 deriving (Eq, Ord, Show, Generic)
-- instance Model Word8 where envType = useCT word8CT

-- instance Model H.Word8 where envType = useCT word8CT


-- instance Flat Word8 where
--   encode (Word8 w) = encode w
--   decode  = Word8 <$> decode
--   size _ n = n+8

