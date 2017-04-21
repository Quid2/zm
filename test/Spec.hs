{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}

module Main where

import           Control.Exception
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Short as SBS
import           Data.Digest.SHA3
import           Data.Either
import           Data.Foldable
import           Data.Int
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Model
import qualified Data.Sequence         as S
import qualified Data.Text             as T
import           Data.Typed
import           Data.Word
import           Debug.Trace
import           Info
import           System.Exit           (exitFailure)
import           System.TimeIt
import           Test.Data             hiding (Unit)
import           Test.Data.Flat        hiding (Unit)
import           Test.Data.Model
import qualified Test.Data2            as Data2
import qualified Test.Data3            as Data3
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Text.PrettyPrint
import Data.Timeless

main = mainTest
-- main = mainMakeTests
-- main = mainPerformance
-- main = mainShow

mainMakeTests = do
  let code = concat ["codes = [",intercalate "\n," $ map (show . typeName) models,"]"]
  putStrLn code
  exitFailure

mainPerformance = do
  print "Calculate codes"
  mapM_ (timeIt . evaluate . typeName) models
  print "Again"
  mapM_ (timeIt . evaluate . typeName) models
  exitFailure

mainShow = do
  -- prt (Proxy::Proxy Char)
  -- prt (Proxy::Proxy String)
  prt (Proxy::Proxy T.Text)
  prt (Proxy::Proxy (BLOB UTF8Encoding))
  -- prt (Proxy::Proxy (Array Word8))
  -- prtH (Proxy::Proxy (Bool,()))
  -- prt (Proxy::Proxy L.ByteString)
  -- prt (Proxy::Proxy T.Text)
  -- print $ tstDec (Proxy::Proxy L.ByteString) [2,11,22,0,1]
  -- print $ tstDec (Proxy::Proxy (Bool,Bool,Bool)) [128+32]
  -- print $ tstDec (Proxy::Proxy (List Bool)) [72]
  print . B.length . flat . timelessSimple $ True
  print . B.length . flat . timelessHash $ True
  print . B.length . flat . timelessExplicit $ True
  print "OK"
  -- prt $ tst (Proxy :: Proxy (List (Data2.List (Bool))))
  exitFailure

    where
      prt = putStrLn . prettyShow . CompactPretty . absTypeModel
      -- pshort = putStrLn . take 1000 . prettyShow

mainTest = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
          [ sha3DigestTests
          , shakeDigestTests
          , codesTests
          , consistentModelTests
          , mutuallyRecursiveTests
          , customEncodingTests
          , encodingTests
          , timelessTests
          ]

sha3DigestTests = testGroup "SHA3 Digest Tests"
                [tst [] [0xa7, 0xff, 0xc6], tst [48, 49, 50, 51] [0x33, 0xbc, 0xc2]]
  where
    tst inp out = testCase (unwords ["SHA3", show inp]) $ B.pack out @?= sha3_256 3 (B.pack inp)

shakeDigestTests = testGroup "Shake Digest Tests"
                [tst [] [0x7f, 0x9c, 0x2b], tst [48, 49, 50, 51] [0x30, 0xc5, 0x1c]]
  where
    tst inp out = testCase (unwords ["Shake128", show inp]) $ shake_128 3 (B.pack inp) @?= B.pack out

codesTests = testGroup "Absolute Types Codes Tests" (map tst $ zip models codes)
  where
    tst (model,code) = testCase (unwords ["Code"]) $ code @?= typeName model

consistentModelTests = testGroup "TypeModel Consistency Tests" $ map tst models
 where
  tst tm = testCase (unwords ["Consistency"]) $ internalConsistency tm && externalConsistency tm @?= True

-- |Check internal consistency of absolute environment
-- all internal references are to entries in the env
internalConsistency at =
  let innerRefs = nub . catMaybes . concatMap (map extRef. toList) . typeADTs $ at
      envRefs = M.keys $ typeEnv at
  in innerRefs `subsetOf` envRefs

subsetOf a b = a \\ b == []

extRef (Ext ref) = Just ref
extRef _         = Nothing

-- |Check external consistency of absolute environment
-- the key of every ADT in the env is correct (same as calculated directly on the ADT)
externalConsistency = all (\(r,adt) -> absRef adt == r) . M.toList . typeEnv

mutuallyRecursiveTests = testGroup "Mutually Recursion Detection Tests" $ [
    tst (Proxy :: Proxy A0)
    ,tst (Proxy :: Proxy B0)
    ,tst (Proxy :: Proxy (Forest Bool))
    ] where
  tst :: forall a. (Model a) => Proxy a -> TestTree
  tst proxy =
    let r = absTypeModelMaybe proxy
    in testCase (unwords ["Mutual Recursion",show r]) $ isLeft r && (let Left e = r in isInfixOf "mutually recursive" e) @?= True

-- |Test all custom flat instances for conformity to their model
customEncodingTests = testGroup "Typed Unit Tests" [
  e ()
  ,e False
  ,e (Just True)
  ,e (Left True::Either Bool Char)
  ,e (Right ()::Either Bool ())
  ,e (M.fromList [(False,'g')])
  ,e (M.fromList [(33::Int,"abc")])
  ,e (M.fromList [(33::Int,57::Word8),(44,77)])
  ,e $ B.pack []
  ,e $ B.pack [11,22]
  ,e $ L.pack []
  ,e $ L.pack (replicate 11 77)
  ,e $ SBS.pack []
  ,e $ SBS.pack [11,22]
  ,e an
  ,e aw
  ,e ab
  ,e $ seq an
  ,e $ seq ab
  ,e $ seq aw
  ,e $ seq ac
  ,e 'k'
  ,e ac
  ,e $ blob NoEncoding [11::Word8,22,33]
  ,e $ blob UTF8Encoding [97::Word8,98,99]
  ,e $ blob UTF16LEEncoding ([0x24,0x00,0xAC,0x20,0x01,0xD8,0x37,0xDC]::[Word8]) -- $€𐐷
  ,e (T.pack "abc$€𐐷")
  ,e (False,True)
  ,e (False,True,44::Word8)
  ,e (False,True,44::Word8,True)
  ,e (False,True,44::Word8,True,False)
  ,e (False,True,44::Word8,True,False,False)
  ,e (False,True,44::Word8,True,False,False,44::Word8)
  ,e (False,True,44::Word8,True,False,False,44::Word8,())
  ,e (False,True,44::Word8,True,False,False,44::Word8,(),'a')
  -- FAILs because of limits in model:Data.Analyse
  -- ,e (False,True,44::Word8,True,False,False,44::Word8,(),Just False,'d')
  ,e (33::Word)
  ,e (0::Word8)
  ,e (33::Word8)
  ,e (255::Word8)
  ,e (3333::Word16)
  ,e (333333::Word32)
  ,e (33333333::Word64)
  ,e (maxBound::Word64)
  ,e (-11111111::Int)
  ,e (11111111::Int)
  ,e (88::Int8)
  ,e (1616::Int16)
  ,e (32323232::Int32)
  ,e (6464646464::Int64)
  ,e (minBound::Int32)
  ,e (maxBound::Int32)
  ,e (minBound::Int64)
  ,e (maxBound::Int64)
  ,e (-88::Int8)
  ,e (-1616::Int16)
  ,e (-32323232::Int32)
  ,e (-6464646464::Int64)
  ,e (44323232123::Integer)
  ,e (-4323232123::Integer)
  -- TODO: doubles
  ,e (12.123::Float)
  ,e (-57.238E-11::Float)
  ]

  where
    seq = S.fromList
    an = []::[()]
    aw = [0,128,127,255::Word8]
    ab = [False,True,False]
    ac = ['v','i','c']

    -- e :: forall a. (Prettier a, Flat a, Show a, Model a) => a -> TestTree
    -- e x = testCase (unwords ["Encoding",show x]) $ dynamicShow x @?= prettierShow x
    e :: forall a. (Pretty a, Flat a, Show a, Model a) => a -> TestTree
    e x = testCase (unwords ["Encoding",show x,prettyShow x,dynamicShow x]) $ dynamicShow x @?= prettyShow x

-- As previous test but using Arbitrary values
encodingTests = testGroup "Encoding Tests"
                  [ ce "()" (prop_encoding :: RT ())
                  , ce "Bool" (prop_encoding :: RT Bool)
                  , ce "Maybe Bool" (prop_encoding :: RT (Maybe Bool))
                  , ce "Word" (prop_encoding :: RT Word)
                  , ce "Word8" (prop_encoding :: RT Word8)
                  , ce "Word16" (prop_encoding :: RT Word16)
                  , ce "Word32" (prop_encoding :: RT Word32)
                  , ce "Word64" (prop_encoding :: RT Word64)
                  , ce "Int" (prop_encoding :: RT Int)
                  , ce "Int16" (prop_encoding :: RT Int16)
                  , ce "Int16" (prop_encoding :: RT Int16)
                  , ce "Int32" (prop_encoding :: RT Int32)
                  , ce "Int64" (prop_encoding :: RT Int64)
                  , ce "Integer" (prop_encoding :: RT Integer)
                  , ce "Char" (prop_encoding :: RT Char)
                  , ce "[Maybe (Bool,Char)]" (prop_encoding :: RT ([Maybe (Bool,Char)]))
                  ]
  where
    ce n = QC.testProperty (unwords ["Encoding", n])

timelessTests = t True
  where
    t n = testGroup (unwords ["Timeless",show n]) [
      testCase "simple" (untimeless (timelessSimple n) @?= Right n)
      ,testCase "hash" (untimeless (timelessHash n) @?= Right n)
      ,testCase "explicit" (untimeless (timelessExplicit n) @?= Right n)
      ]

-- prop_encoding :: forall a. (Prettier a,Flat a, Show a, Model a) => RT a
-- prop_encoding x = dynamicShow x == prettierShow x
-- dynamicShow :: forall a. (Prettier a,Flat a, Show a, Model a) => a -> String
-- dynamicShow a = prettyShow (let Right v = decodeAbsTypeModel (absTypeModel (Proxy::Proxy a)) (flat a) in v)

prop_encoding :: forall a. (Pretty a,Flat a, Model a) => RT a
prop_encoding x = dynamicShow x == prettyShow x

dynamicShow :: forall a. (Flat a, Model a) => a -> String
dynamicShow a = prettyShow (let Right v = decodeAbsTypeModel (absTypeModel (Proxy::Proxy a)) (flat a) in v)

type RT a = a -> Bool



