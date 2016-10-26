{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Main where

import           Control.Applicative
import           Data.Bifunctor
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as L
import           Data.Digest.SHA3
import           Data.Foldable
import           Data.Int
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Model
import qualified Data.Text             as T
import           Data.Typed
import qualified Data.Typed.PrimTypes  as P
import           Data.Word
import           Debug.Trace
import           System.Exit           (exitFailure)
import           Test.Data hiding (Unit)
import           Test.Data.Flat hiding (Unit)
import           Test.Data.Model
import qualified Test.Data2            as Data2
import qualified Test.Data3            as Data3
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

-- import qualified P.Word7
-- check that a type is encoded according to its declared type.
-- encode then decode accord to type then
-- Val 1 "False"
t = main

main = mainT

mainT = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties
                           ,digestTests
                           ,zmTests
                           ,unitTests]

properties = testGroup "Typed Properties"
  [
  --   ce "()" (prop_encoding :: RT ())
  --   ,ce "Bool" (prop_encoding :: RT Bool)
  --   ,ce "Maybe Bool" (prop_encoding :: RT (Maybe Bool))
  ]
   where ce n = QC.testProperty (unwords ["encoding",n])

prop_encoding :: forall a. (Flat a, Show a, Model a) => RT a
prop_encoding x = prettyShow (decodeAbsType (traceShowId (absoluteType (Proxy::Proxy a))) (flat x)) == show x

-- absoluteType ()

digestTests = testGroup "Digest Tests" [
  tst [] [0xa7,0xff,0xc6]
  ,tst [48,49,50,51] [0x33,0xbc,0xc2]
  ] where
    tst inp out = testCase (unwords ["SHA3",show inp]) $ B.pack out @?= sha3_256 3 (B.pack inp)

zmTests = testGroup "ZhengMing Tests" [
  tst (Proxy::Proxy Word32)
  ,tst (Proxy::Proxy AbsADT)
  ] where
  tst proxy = testCase (unwords ["Consistency"]) $ internalConsistency proxy && externalConsistency proxy @?= True
  --tstIn proxy = testCase (unwords ["Internal Consistency"]) $ internalConsistency proxy @?= True
    --tstOut proxy = testCase (unwords ["Internal Consistency"]) $ internalConsistency proxy @?= True

-- CHECK internal consistency of env (all internal references are to entries in the env)
internalConsistency :: Model a => Proxy a -> Bool
internalConsistency proxy =
  let at = absoluteType proxy
      innerRefs = nub . catMaybes . concatMap (map extRef. toList) $ M.elems $ canonicalEnv at
      envRefs = M.keys $ canonicalEnv at
  in innerRefs \\ envRefs == []

extRef (Ext ref) = Just ref
extRef _ = Nothing

-- Check external consistency, the key of every ADT in the env is the same as calculated on the ADT
externalConsistency :: Model a => Proxy a -> Bool
externalConsistency = all (\(r,adt) -> refS adt == r) . M.toList . canonicalEnv . absoluteType

unitTests = testGroup "Typed Unit Tests" [
  -- Test all custom flat instances for conformity to their model
  e ()
  ,e False
  ,e (Just True)
  ,e (Left True::Either Bool Char)
  ,e (Right ()::Either Bool ())
  ,e $ B.pack []
  ,e $ B.pack [11,22]
  ,e $ L.pack []
  ,e $ L.pack (replicate 11 77)
  ,e an
  ,e aw
  ,e ab
  ,e $ Array an
  ,e $ Array ab
  ,e $ Array aw
  ,e $ Array ac
  ,e 'k'
  ,e ac
  ,e (T.pack "abc")
  --,e $ blob UTF8Encoding (L.pack [97,98,99])
  ,e (False,True)
  ,e (False,True,44::Word8)
  ,e (False,True,44::Word8,True)
  ,e (False,True,44::Word8,True,False)
  -- FAILs because of limits in model:Data.Analyse
  --,e (False,True,44::Word8,True,False,False)
  --,e (False,True,44::Word8,True,False,False,44::Word8)
  -- ,e (False,True,44::Word8,True,False,False,44::Word8,())
  -- ,e (False,True,44::Word8,True,False,False,44::Word8,(),Just False)
  -- ,e (33::Word)
  ,e (33::Word8)
  ,e (3333::Word16)
  ,e (333333::Word32)
  ,e (33333333::Word64)
  ,e (88::Int8)
  ,e (1616::Int16)
  ,e (32323232::Int32)
  ,e (6464646464::Int64)
  ,e (11111111::Int)
  ,e (-88::Int8)
  ,e (-1616::Int16)
  ,e (-32323232::Int32)
  ,e (-6464646464::Int64)
  ,e (-11111111::Int)
  ,e (44323232123::Integer)
  ,e (-4323232123::Integer)
  -- TODO: floats
  ]

  where
    an = []::[()]
    aw = [0,128,127,255::Word8]
    ab = [False,True,False]
    ac = ['v','i','c']
    e :: forall a. (Prettier a, Flat a, Show a, Model a) => a -> TestTree
    e x = testCase (unwords ["Encoding",show x]) $ prettyShow (decodeAbsType (absoluteType (Proxy::Proxy a)) (flat x)) @?= render (prettier x)

type RT a = a -> Bool

mainP = do
  -- print $ L.unpack . flat $ L.pack [11,22]
  -- print $ flat $ Array [11,22::Word8]
  -- print $ flat True
  -- print $ L.unpack $ flat (C True (C False N))
  -- prt (Proxy::Proxy Char)
  print $ L.unpack . flat $ T.pack "abc"
  -- prt (Proxy::Proxy String)
  prt (Proxy::Proxy T.Text)
  prt (Proxy::Proxy (BLOB UTF8Encoding))
  -- prt (Proxy::Proxy (Array Word8))
  -- prt (Proxy::Proxy ([Bool]))
  -- prt (Proxy::Proxy L.ByteString)
  -- prtH (Proxy::Proxy (Bool,()))
  -- prt (Proxy::Proxy L.ByteString)
  -- prt (Proxy::Proxy T.Text)
  -- print $ tstDec (Proxy::Proxy L.ByteString) [2,11,22,0,1]
  -- print $ tstDec (Proxy::Proxy (Bool,Bool,Bool)) [128+32]
  -- print $ tstDec (Proxy::Proxy (List Bool)) [72]
  print "OK"
  -- prt $ tst (Proxy :: Proxy (List (Data2.List (Bool))))
  exitFailure
-- [3,119,119,119,0,1,7,72,97,115,107,101,108,108,0,0,0,237,13,13,13,13,13,13,0,0,0,0,0,0,0,0]
-- tstDec :: Val
    where
      prt = putStrLn . take 1000 . prettyShow . absoluteType
      prtH = putStrLn . take 1000 . prettyShow . hTypeEnv

tstDec proxy bs =
     let t@(AbsoluteType absEnv absType) = absoluteType proxy
         decEnv = typeDecoderEnv (traceShowId absEnv) (traceShowId absType)
         dec = typeDecoder decEnv absType
         v = runGet dec (L.pack bs)
     in trace (prettyShow t) v

-- Word8 = TypeCon (Shake128 (Cons 142 (Cons 41 (Cons 150 (Elem 138)))))
-- Word7 = TypeCon (Shake128 (Cons 99 (Cons 171 (Cons 248 (Elem 77)
main3 = do
  -- print $ tst (Proxy :: Proxy P.Word7)
  -- print $ tst (Proxy :: Proxy Word8)
  -- TypeCon (Shake128 (Cons 50 (Cons 156 (Cons 252 (Elem 175)))))
  -- prt $ tst (Proxy :: Proxy P.String)
   -- TypeApp (TypeCon (Shake128 (Cons 62 (Cons 76 (Cons 155 (Elem 130)))))) (TypeCon (Shake128 (Cons 239 (Cons 54 (Cons 53 (Elem 175)))
  -- prt $ tst (Proxy :: Proxy P.String)
  prt $ tst (Proxy :: Proxy (P.Array Bool))
  -- prt $ tst (Proxy :: Proxy Char)
  -- prt $ tst (Proxy :: Proxy [Word8])
  print 4
  exitFailure

tst = absoluteType

main2 =  do
  mapM prt [
     tst (Proxy :: Proxy Bool)
    ,tst (Proxy :: Proxy (List Bool))
    ,tst (Proxy :: Proxy (Data2.List Bool))
    ,tst (Proxy :: Proxy (Data3.List Bool))
    ,tst (Proxy :: Proxy (List (Data2.List (Data3.List Bool))))
    --,tst (Proxy :: Proxy (Forest Bool))
    ,tst (Proxy :: Proxy (Forest2 Bool))
     ,tst (Proxy :: Proxy AbsADT)
     --tst (Proxy :: Proxy Q.Word7)
    -- tst (Proxy :: Proxy Char)
    -- ,tst (Proxy :: Proxy String)
    ,tst (Proxy :: Proxy Word8)
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

prt :: AbsoluteType -> IO ()
prt (AbsoluteType e t) = do
  putStrLn ""
  -- print (t,e)
  mapM_ (putStrLn . take 1000 . prettyShow . (e,)) . M.elems $ e

-- Example of overrides of absType
-- x = absType (Proxy::Proxy Char)
-- xx = map (const $ absType (Proxy::Proxy Char)) [1..10]
-- y = absType (Proxy::Proxy (Char,Word8))

-- instance Typed Char where absType _ = TypeCon (Shake128 (Cons 66 (Cons 88 (Cons 111 (Elem 111)))))

-- instance (Model a,Model b) => Typed (a,b) where absType _ = TypeApp (TypeApp (TypeCon (Shake128 (Cons 46 (Cons 153 (Cons 123 (Elem 78)))))) (absType (Proxy::Proxy a))) (absType (Proxy::Proxy b))

instance {-# OVERLAPS #-} Pretty [Word8] where pPrint = text . show
instance {-# OVERLAPS #-} Pretty [Bool] where pPrint = text . show

class Prettier a where prettier :: a -> Doc
instance {-# OVERLAPPABLE #-} Pretty a => Prettier a where prettier = pPrint
-- instance Show a => Prettier a where prettier = text . show
instance {-# OVERLAPPABLE #-} Prettier a => Prettier [a] where prettier = prettyList prettier

instance (Prettier a1,Prettier a2) => Prettier (a1,a2) where prettier (a1,a2) = prettyTuple [prettier a1,prettier a2]
instance (Prettier a1,Prettier a2,Prettier a3) => Prettier (a1,a2,a3) where prettier (a1,a2,a3) = prettyTuple [prettier a1,prettier a2,prettier a3]
instance (Prettier a1,Prettier a2,Prettier a3,Prettier a4) => Prettier (a1,a2,a3,a4) where prettier (a1,a2,a3,a4) = prettyTuple [prettier a1,prettier a2,prettier a3,prettier a4]
instance (Prettier a1,Prettier a2,Prettier a3,Prettier a4,Prettier a5) => Prettier (a1,a2,a3,a4,a5) where prettier (a1,a2,a3,a4,a5) = prettyTuple [prettier a1,prettier a2,prettier a3,prettier a4,prettier a5]
instance (Prettier a1,Prettier a2,Prettier a3,Prettier a4,Prettier a5,Prettier a6) => Prettier (a1,a2,a3,a4,a5,a6) where prettier (a1,a2,a3,a4,a5,a6) = prettyTuple [prettier a1,prettier a2,prettier a3,prettier a4,prettier a5,prettier a6]
instance (Prettier a1,Prettier a2,Prettier a3,Prettier a4,Prettier a5,Prettier a6,Prettier a7) => Prettier (a1,a2,a3,a4,a5,a6,a7) where prettier (a1,a2,a3,a4,a5,a6,a7) = prettyTuple [prettier a1,prettier a2,prettier a3,prettier a4,prettier a5,prettier a6,prettier a7]
instance (Prettier a1,Prettier a2,Prettier a3,Prettier a4,Prettier a5,Prettier a6,Prettier a7,Prettier a8) => Prettier (a1,a2,a3,a4,a5,a6,a7,a8) where prettier (a1,a2,a3,a4,a5,a6,a7,a8) = prettyTuple [prettier a1,prettier a2,prettier a3,prettier a4,prettier a5,prettier a6,prettier a7,prettier a8]
instance (Prettier a1,Prettier a2,Prettier a3,Prettier a4,Prettier a5,Prettier a6,Prettier a7,Prettier a8,Prettier a9) => Prettier (a1,a2,a3,a4,a5,a6,a7,a8,a9) where prettier (a1,a2,a3,a4,a5,a6,a7,a8,a9) = prettyTuple [prettier a1,prettier a2,prettier a3,prettier a4,prettier a5,prettier a6,prettier a7,prettier a8,prettier a9]

instance {-# OVERLAPS #-} Prettier String where prettier s = text . concat $ ["\"",s,"\""]
instance {-# OVERLAPS #-} Prettier Char where prettier c = text ['\'',c,'\'']

instance Prettier a => Prettier (Array a) where prettier (Array vs) = arr_ prettier vs
instance Show e => Pretty (BLOB e) where pPrint b = text . show $ b

instance Prettier Unit where prettier _ = text "()"

instance Prettier Word where prettier = text . show
instance Prettier Word16 where prettier = text . show
instance Prettier Word32 where prettier = text . show
instance Prettier Word64 where prettier = text . show
instance Prettier Int where prettier = text . show
instance Prettier Int8 where prettier = text . show
instance Prettier Int16 where prettier = text . show
instance Prettier Int32 where prettier = text . show
instance Prettier Int64 where prettier = text . show
instance Prettier Integer where prettier = text . show

instance Prettier B.ByteString where prettier = arr_ (int . fromIntegral) . B.unpack
instance Prettier L.ByteString where prettier = arr_ (int . fromIntegral) . L.unpack

-- ar_ :: (a -> Doc) -> [a] -> Doc
-- ar_ f elems = char '[' <> (hcat . intersperse (char ',') . map f $ elems) <> char ']'

arr_ :: (a -> Doc) -> [a] -> Doc
arr_ f elems = hsep $ text "Array" : [prettyList f elems]
