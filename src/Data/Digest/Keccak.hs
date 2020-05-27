{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE PackageImports           #-}

-- |Crypto algorithms of the Keccak family (SHA3/SHAKE), with support for GHCJS.
module Data.Digest.Keccak (sha3_256, shake_128) where

import qualified Data.ByteString         as B

#ifdef ghcjs_HOST_OS

import           GHCJS.Marshal
import           GHCJS.Types
import           System.IO.Unsafe

#else

import qualified "cryptonite" Crypto.Hash             as S
import qualified Data.ByteArray          as S
#endif

--import qualified Data.ByteString         as B


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import ZM.Pretty
-- >>> import Data.String
-- >>> 

{- |Return the specified number of bytes of the SHAKE-128 hash of the provided byte string
>>> shake_128 8 B.empty == B.pack [127, 156, 43, 164, 232, 143, 130, 125]
True

>>> shake_128 32 (B.pack [1..10]) == B.pack [142,56,168,122,207,188,35,211,233,209,95,158,63,91,102,156,114,204,22,38,177,105,130,116,173,114,190,153,159,101,10,150]
True

>>> let i = B.pack [1..10] in shake_128 4 i == B.take 4 (shake_128 32 i)
True
-}
shake_128 :: Int -> B.ByteString -> B.ByteString
shake_128 numBytes bs | numBytes <=0 || numBytes > 32 = error "shake128: Invalid number of bytes"
                      | otherwise = shake_128_ numBytes bs

-- |Return the specified number of bytes of the SHA-3 hash of the provided byte string
sha3_256 :: Int -> B.ByteString -> B.ByteString
sha3_256 numBytes bs | numBytes <=0 || numBytes > 32 = error "sha3_256: Invalid number of bytes"
                     | otherwise = sha3_256_ numBytes bs

#ifdef ghcjs_HOST_OS

-- CHECK: is it necessary to pack/unpack the ByteStrings?
shake_128_ :: Int -> B.ByteString -> B.ByteString
shake_128_ numBytes = stat (js_shake128 $ numBytes*8) numBytes -- 256)

sha3_256_ :: Int -> B.ByteString -> B.ByteString
sha3_256_ = stat js_sha3_256

stat :: (JSVal -> JSVal) -> Int -> B.ByteString -> B.ByteString
stat f n bs = unsafePerformIO $ do
   jbs <- toJSVal $ B.unpack $ bs
   Just bs' <- fromJSVal $ f jbs
   return . B.take n . B.pack $ bs'

-- PROB: these references will be scrambled by the `closure` compiler, as they are not static functions but are setup dynamically by the sha3.hs library
foreign import javascript unsafe "shake_128.array($2, $1)" js_shake128 :: Int -> JSVal -> JSVal

--foreign import javascript unsafe "sha3_224.array($1)" js_sha3_224 :: JSVal -> JSVal

foreign import javascript unsafe "sha3_256.array($1)" js_sha3_256 :: JSVal -> JSVal

-- foreign import javascript unsafe "keccak_256.array($1)" js_keccak256 :: JSVal -> JSVal
-- foreign import javascript unsafe "(window == undefined ? global : window)['keccak_256']['array']($1)" js_keccak256 :: JSVal -> JSVal

#else

shake_128_ :: Int -> B.ByteString -> B.ByteString
shake_128_ = stat (S.SHAKE128 :: S.SHAKE128 256)

sha3_256_ :: Int -> B.ByteString -> B.ByteString
sha3_256_ = stat S.SHA3_256

stat
  :: (S.ByteArrayAccess ba, S.HashAlgorithm alg) =>
     alg -> Int -> ba -> B.ByteString
stat f numBytes = B.take numBytes . S.convert . S.hashWith f

#endif
