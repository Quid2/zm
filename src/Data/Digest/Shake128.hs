module Data.Digest.Shake128(shake128) where
import qualified Data.ByteString    as B
import qualified Crypto.Hash.SHA3 as S
-- fake
s1 n = shake128 n $ B.pack [3,42,123,53,55]

shake128 :: Int -> B.ByteString -> B.ByteString
shake128 numBytes bs | numBytes <=0 || numBytes > 32 = error "shake128: Invalid number of bytes"
                      | otherwise = B.take numBytes $ S.hash 256 bs
