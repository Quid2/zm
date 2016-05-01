{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Main where

import           Control.Applicative
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as L
import           Data.Foldable
import           Data.Int
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Model
import           Data.Typed
import           Data.Word
import           Debug.Trace
import           System.Exit          (exitFailure)
import           Test.Data
import           Test.Data.Flat
import           Test.Data.Model
import qualified Test.Data2           as Data2
import qualified Test.Data3           as Data3

t = main

main = do
  print $ L.unpack $ flat (C True (C False N))
  print $ tstDec (Proxy::Proxy (Bool,Bool,Bool)) [128+32]
  print . prettyShow $ tstDec (Proxy::Proxy (List Bool)) [72]
  print "OK"
  -- prt $ tst (Proxy :: Proxy (List (Data2.List (Bool))))
  exitFailure
-- [3,119,119,119,0,1,7,72,97,115,107,101,108,108,0,0,0,237,13,13,13,13,13,13,0,0,0,0,0,0,0,0]
-- tstDec :: Val
tstDec proxy bs =
     let (absType,absEnv) = absTypeEnv proxy
         decEnv = typeDecoderEnv (traceShowId absEnv) (traceShowId absType)
         dec = typeDecoder decEnv absType
         v = runGet dec (L.pack bs)
     in v


tst = absoluteType

main2 =  do
  mapM prt [
     tst (Proxy :: Proxy Bool)
    ,tst (Proxy :: Proxy (List Bool))
    ,tst (Proxy :: Proxy (Data2.List Bool))
    ,tst (Proxy :: Proxy (Data3.List Bool))
    ,tst (Proxy :: Proxy (List (Data2.List (Data3.List Bool))))
    ,tst (Proxy :: Proxy (Forest Bool))
    ,tst (Proxy :: Proxy (Forest2 Bool))
    ,tst (Proxy :: Proxy AbsADT)
     --tst (Proxy :: Proxy Q.Word7)
    -- tst (Proxy :: Proxy Char)
    -- ,tst (Proxy :: Proxy String)
    -- ,tst (Proxy :: Proxy Word8)
    -- ,tst (Proxy :: Proxy Word16)
    -- ,tst (Proxy :: Proxy Int8)
    -- ,tst (Proxy :: Proxy Int16)
    --  --,tst (Proxy: Proxy D2)
    -- ,tst (Proxy :: Proxy D4)
    -- ,tst (Proxy :: Proxy A0)
    -- ,tst (Proxy :: Proxy B0)
    -- ,tst (Proxy :: Proxy (Phantom Unit))
    -- ,tst (Proxy :: Proxy (List Bool))
    -- ,tst (Proxy :: Proxy (Either Bool Unit))
    -- ,tst (Proxy :: Proxy (Either Bool Bool))
    -- ,tst (Proxy :: Proxy (RR Un Unit N))
    ]
  exitFailure

prt o = do
  putStrLn ""
  print o
  mapM (putStrLn . prettyShow) . M.elems . snd $ o


-- Example of overrides of absType
-- x = absType (Proxy::Proxy Char)
-- xx = map (const $ absType (Proxy::Proxy Char)) [1..10]
-- y = absType (Proxy::Proxy (Char,Word8))

-- instance Typed Char where absType _ = TypeCon (Shake128 (Cons 66 (Cons 88 (Cons 111 (Elem 111)))))

-- instance (Model a,Model b) => Typed (a,b) where absType _ = TypeApp (TypeApp (TypeCon (Shake128 (Cons 46 (Cons 153 (Cons 123 (Elem 78)))))) (absType (Proxy::Proxy a))) (absType (Proxy::Proxy b))

