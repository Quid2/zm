{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP, EmptyDataDecls #-}
module Data.Digest.Shake128(shake128) where

import qualified Data.ByteString    as B

#ifdef ghcjs_HOST_OS
import System.IO.Unsafe
import GHCJS.Types
import GHCJS.Marshal
#else
import qualified Crypto.Hash as S
-- import qualified Crypto.Hash.Types as S
import qualified Data.ByteArray as S
import qualified Data.ByteArray.Encoding as S
#endif

t = s1 == B.pack [88,129,9,45,216,24,191,92]

t2 = s2 == B.pack [127,156,43,164,232,143,130,125,97,96,69,80,118,5,133,62,215,59,128,147,246,239,188,136,235,26,110,172,250,102,239,38]

-- sha3 haskell
-- s1 [78,3,101,122,234,69,169,79]
-- s2 [197,210,70,1,134,247,35,60,146,126,125,178,220,199,3,192,229,0,182,83,202,130,39,59,123,250,216,4,93,133,164,112]
s1 = shake128 8 (B.pack [97,98,99])
s2 = shake128 32 (B.pack [])

shake128 :: Int -> B.ByteString -> B.ByteString
shake128 numBytes bs | numBytes <=0 || numBytes > 32 = error "shake128: Invalid number of bytes"
                     | otherwise = shake128_ numBytes bs

#ifdef ghcjs_HOST_OS

-- CHECK: is it necessary to pack/unpack the ByteStrings?
shake128_ :: Int -> B.ByteString -> B.ByteString
shake128_ n bs = unsafePerformIO $ do
   jbs <- toJSVal $ B.unpack $ bs
   -- Just bs' <- fromJSVal $ js_shake128 (n*8) jbs
   -- return . B.pack $ bs'
   Just bs' <- fromJSVal $ js_keccak256 jbs
   return . B.take n . B.pack $ bs' -- return $ toBS1 $ js_shake128 (n*8) jbs

-- PROB: these references will be scrambled by the closure compiler, as they are not static functions but are setup dynamically by the sha3.hs library
foreign import javascript unsafe "shake_128.array($2, $1)" js_shake128 :: Int -> JSVal -> JSVal

foreign import javascript unsafe "sha3_256.array($1)" js_sha3 :: JSVal -> JSVal

foreign import javascript unsafe "keccak_256.array($1)" js_keccak256 :: JSVal -> JSVal
-- foreign import javascript unsafe "(window == undefined ? global : window)['keccak_256']['array']($1)" js_keccak256 :: JSVal -> JSVal
#else

-- fake implementation
shake128_ :: Int -> B.ByteString -> B.ByteString
shake128_ numBytes = B.take numBytes . S.convert . S.hashWith S.Keccak_256

#endif
