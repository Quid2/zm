{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE PackageImports           #-}
-- |Crypto algorithms (with support for GHCJS)
module Data.Digest.SHA3(
  sha3_256
  ) where

import qualified Data.ByteString         as B

#ifdef ghcjs_HOST_OS
import           GHCJS.Marshal
import           GHCJS.Types
import           System.IO.Unsafe
#else
import qualified "cryptonite" Crypto.Hash             as S
import qualified Data.ByteArray          as S
import qualified Data.ByteArray.Encoding as S
#endif

-- keccak256_6 = shake128 6

-- shake128 :: Int -> B.ByteString -> B.ByteString
-- shake128 numBytes bs | numBytes <=0 || numBytes > 32 = error "shake128: Invalid number of bytes"
--                      | otherwise = shake128_ numBytes bs

sha3_256 :: Int -> B.ByteString -> B.ByteString
sha3_256 numBytes bs | numBytes <=0 || numBytes > 32 = error "sha3_256: Invalid number of bytes"
                     | otherwise = sha3_256_ numBytes bs

#ifdef ghcjs_HOST_OS

-- CHECK: is it necessary to pack/unpack the ByteStrings?
-- shake128_ :: Int -> B.ByteString -> B.ByteString
-- shake128_ = stat js_keccak256

sha3_256_ :: Int -> B.ByteString -> B.ByteString
sha3_256_ = stat js_sha3_256

stat f n bs = unsafePerformIO $ do
   jbs <- toJSVal $ B.unpack $ bs
   -- Just bs' <- fromJSVal $ js_shake128 (n*8) jbs
   -- return . B.pack $ bs'
   Just bs' <- fromJSVal $ f jbs
   return . B.take n . B.pack $ bs' -- return $ toBS1 $ js_shake128 (n*8) jbs

-- PROB: these references will be scrambled by the closure compiler, as they are not static functions but are setup dynamically by the sha3.hs library
foreign import javascript unsafe "shake_128.array($2, $1)" js_shake128 :: Int -> JSVal -> JSVal

foreign import javascript unsafe "sha3_224.array($1)" js_sha3_224 :: JSVal -> JSVal

foreign import javascript unsafe "sha3_256.array($1)" js_sha3_256 :: JSVal -> JSVal

foreign import javascript unsafe "keccak_256.array($1)" js_keccak256 :: JSVal -> JSVal
-- foreign import javascript unsafe "(window == undefined ? global : window)['keccak_256']['array']($1)" js_keccak256 :: JSVal -> JSVal

#else

-- fake implementation
-- shake128_ :: Int -> B.ByteString -> B.ByteString
-- shake128_ = stat S.Keccak_256

sha3_256_ :: Int -> B.ByteString -> B.ByteString
sha3_256_ = stat S.SHA3_256

stat f numBytes = B.take numBytes . S.convert . S.hashWith f

#endif
