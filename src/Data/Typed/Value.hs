{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Data.Typed.Value(typedBytes,untypedBytes
                       ,typedValue,untypedValue
                       ,TypeDecoders,typeDecoderEnv,typeDecoder,decodeAbsType
                       ,timelessSimple,timelessAbs,timelessExplicit
                       ,TypeMatchers,BTree(..),matchTree) where

import           Control.Monad.Trans.State
import           Data.Binary.Bits.Get      (Get, getBool, runGet, runPartialGet)
import qualified Data.ByteString.Lazy      as L
import           Data.Flat
import           Data.Foldable
import qualified Data.ListLike.String      as L
import qualified Data.Map                  as M
import           Data.Model
import           Data.Typed.Class
import           Data.Typed.Instances
import           Data.Typed.Pretty
import           Data.Typed.Transform
import           Data.Typed.Types
import           Data.Word
-- import           Debug.Trace
traceShowId = id

bb = flat . typedBytes $ (True,False,True)

rr = (flat . timelessExplicit $ [True],flat . timelessAbs $ [True])

b :: Decoded (Bool,Bool,Bool)
b = untypedBytes . unflat . flat . typedBytes $ (True,False,True)

x :: Decoded Bool
x = untimeless . unflat . flat . timelessSimple $ True

xw :: Decoded Word8
xw = untimeless . unflat . flat . timelessSimple $ True

-- y = unTimeless . unflat $ k1

y :: Decoded Bool
y = untimeless . unflat . flat . timelessAbs $ True

d = (unflat . flat . timelessAbs $ True) >>= untimelessDynamic_

-- f = flat . timeless $ True

k1 = L.pack [174,179,12,6,31,46,239,67,229,118,63,213,192,121,7,107,40,191,124,94,28,1,0,1,1,129,0,1]

-- timeless :: forall a . (Model a, Flat a) => a -> Timeless
-- timeless a = Timeless (typedBytes (absType (Proxy :: Proxy a))) (blob FlatEncoding . flat $ a)

-- Fake type system
data SimpleType = BoolType | CharType deriving (Eq, Ord, Show, Generic)
instance Model SimpleType
instance Flat SimpleType

-- ex: True :: TypeCon () :: Type AbsRef
-- ex: True :: BoolType :: SimpleType
timelessAbs :: forall a . (Model a, Flat a) => a -> Timeless
timelessAbs a = Timeless (typedBytes (absType (Proxy :: Proxy a))) (blob FlatEncoding . flat $ a)

timelessExplicit :: forall a . (Model a, Flat a) => a -> Timeless
timelessExplicit a = Timeless (typedBytes (absoluteType (Proxy :: Proxy a))) (blob FlatEncoding . flat $ a)

timelessSimple :: Bool -> Timeless
timelessSimple a = Timeless (typedBytes BoolType) (blob FlatEncoding . flat $ a)

-- WARN: adds additional end alignment byte
typedBytes :: forall a . (Typed a,Flat a) => a -> TypedBLOB
typedBytes v = TypedBLOB (absType (Proxy :: Proxy a)) (blob FlatEncoding . flat $ v)

typedValue :: forall a . Typed a => a -> TypedValue a
typedValue = TypedValue (absType (Proxy :: Proxy a))

{-
Dynamically recover a value, discover metamodel of a data type, its type

Reqs:
-- Include metamodel
-- Avoid duplication
-}

-- meta = TypeApp "Type" (TyCon "Keccak") :: Type Shake128
-- Meta must be of a known type ?
-- or it could be a Blob of known size so that we can compare it with known signatures?
-- tbs = TypeCon "Bool" :: Type Keccak
-- value = True :: Bool

untimeless ::  forall a.  (Flat a, Model a) => Decoded Timeless -> Decoded a
untimeless dt = dt >>= untimeless_

untimelessDynamic_ :: Timeless -> Decoded (L.ByteString,AbsType, AbsType)
untimelessDynamic_ (Timeless (TypedBLOB meta tbs) bs) =
  if meta == metaAbsType
  then (unblob bs,,meta) <$> (unflat . unblob $ tbs :: Decoded AbsType)
  else Left "unknown meta model"

untimeless_ ::  forall a.  (Flat a, Model a) => Timeless -> Decoded a
untimeless_ (Timeless (TypedBLOB meta tbs) bs) =
  let expectedType = absType (Proxy :: Proxy a)
  in if meta == metaAbsType
     then let Right (actualType :: AbsType) = unflat . unblob $ tbs
          in if expectedType == actualType
             then unflat . unblob $ bs
             else typeErr expectedType actualType
     else if meta == metaSimpleType
          then let Right (simpleType :: SimpleType) = unflat . unblob $ tbs
                   actualType = if simpleType == BoolType then metaBool else metaChar
               in if expectedType == actualType
                  then unflat . unblob $ bs
                  else typeErr expectedType actualType -- Left "SimpleType can only be a Boolean or a Char"
          else Left "unknown meta model"

metaAbsType :: AbsType
metaAbsType = absType (Proxy :: Proxy AbsType)

metaExplicitType :: AbsType
metaExplicitType = absType (Proxy :: Proxy AbsoluteType)

metaSimpleType :: AbsType
metaSimpleType = absType (Proxy :: Proxy SimpleType)

metaBool :: AbsType
metaBool = absType (Proxy :: Proxy Bool)

metaChar :: AbsType
metaChar = absType (Proxy :: Proxy Char)

-- funKeccak = ta $ absType (Proxy :: Proxy (Type Keccak256_6))
-- t2 = let TypeApp f a = absType (Proxy :: Proxy (Type Shake128)) in f

fun ta = let TypeApp f a = ta in f

untypedBytes ::  forall a.  (Flat a, Model a) => Either DeserializeFailure TypedBLOB -> Either DeserializeFailure a
untypedBytes ea = case ea of
                    Left e -> Left e
                    Right (TypedBLOB typ' bs) ->
                      let typ = absType (Proxy :: Proxy a)
                      in if (typ' /= typ)
                         then typeErr typ typ'
                         else unflat . unblob $ bs

untypedValue ::  (Flat a, Model a) => Either DeserializeFailure (TypedValue a) -> Either DeserializeFailure a
untypedValue ea = case ea of
                    Left e -> Left e
                    Right (TypedValue typ' a) ->
                      let typ = absType (proxyOf a)
                      in if (typ' /= typ)
                         then typeErr typ typ'
                         else Right a

typeErr typ typ' =
  let rt  = prettyShow typ
      rt' = prettyShow typ'
      msg = unwords $ ["Was expecting type:\n",rt,"\n\nBut the data has type:\n",rt']
      -- putStrLn msg
  in Left msg

-- Dynamic decoding
-- x :: Val
--x = decodeAbsType (absoluteType (Proxy::Proxy (Bool,Bool,Bool))) (L.pack [128+32])
-- x = decodeAbsType (absoluteType (Proxy::Proxy Bool)) (L.pack [128+32])
-- x = decodeAbsType (absoluteType (Proxy::Proxy Char)) (flat 'z')
-- x = decodeAbsType (absoluteType (Proxy::Proxy String)) (flat "bhu")
--x = decodeAbsType (absoluteType (Proxy::Proxy [Bool])) (flat $ [True,False,True])
-- x = decodeAbsType (absoluteType (Proxy::Proxy Word8)) (flat $ 44::Word8

p = mapM_ putStrLn [--tst (),
                    tst(False,'x')
                   ,tst True,tst 'j',tst "",tst "abc",tst (123::Word8),tst (12345::Word16),tst (123456::Word32),tst (123456789::Word64),tst (123456::Word)]

tst :: forall a . (Model a,Flat a) => a -> String
tst v = prettyShow $ decodeAbsType (absoluteType (Proxy::Proxy a)) (flat v)

dd = matchTree (absoluteType (Proxy::Proxy Word8)) --  [Bool])) -- [Bool])

type TypeMatchers = M.Map AbsType BTree

matchTree :: AbsoluteType -> TypeMatchers
matchTree at =
  let -- at = absoluteType proxy
      decEnv = typeDecoderEnv (canonicalEnv at) (canonicalType at)
  --in map typeTree decEnv (canonicalType at)  (canonicalEnv at)
  in M.mapWithKey (\t _ -> simplifyBTree $ typeTree decEnv t) decEnv

-- matchTree2 proxy =
--   let at = absoluteType proxy
--       decEnv = typeDecoderEnv (canonicalEnv at) (canonicalType at)
--   in decEnv

-- CHECK: ignores left over bits
decodeAbsType :: AbsoluteType -> L.ByteString -> Val
decodeAbsType at =
  let
    decEnv = typeDecoderEnv (traceShowId $ canonicalEnv at) (traceShowId $ canonicalType at)
    dec = typeDecoder decEnv (canonicalType at)
    in runGet dec

-- A table of decoders, one for every relevant type
type TypeDecoders = M.Map (Type AbsRef) (ConTree Identifier AbsRef)
typeDecoderEnv :: ADTEnv -> Type AbsRef -> TypeDecoders
typeDecoderEnv adtEnv absType = execEnv (addType adtEnv absType)

-- Insert in the env the saturated constructor trees corresponding to the passed type
-- and any type nested in its definition
addType absEnv t = do
  mct <- M.lookup t <$> get
  case mct of
    Nothing -> do
       -- Note: we assume that the datatype has values
       let Just ct = declCons $ solvedADT absEnv t
       modify (M.insert t ct)
       -- Recursively on all saturated types inside the contructor tree
       mapM_ (addType absEnv) (conTreeTypeList ct)
    Just _ -> return ()

-- data BTree = BFork BTree BTree | BCon [BTree] deriving (Show,Eq)
data BTree = BFork BTree BTree
           | BCon [AbsType]
           | Skip Int
           deriving (Show,Eq)

simplifyBTree :: BTree -> BTree
simplifyBTree = rec sym

rec :: (BTree -> Maybe BTree) -> BTree -> BTree
rec f n@(BFork l r) =
      case f n of
        Nothing -> let n = BFork (rec f l) (rec f r)
                   in case f n of
                        Nothing -> n
                        Just nn -> nn
        Just nn -> nn

rec f n = case f n of
      Nothing -> n
      Just nn -> nn

sym (BFork (BCon []) (BCon [])) = Just $ Skip 1
sym (BFork (Skip n1) (Skip n2)) | n1 == n2 = Just $ Skip (n1+1)
sym _ = Nothing

sl (BFork (BCon []) (BCon [])) = Just 1
sl (BFork l r) =( (,) <$> sl l <*> sl r) >>= snxt
snxt (n1,n2) | n1 == n2 = Just (n1+1)
snxt _ = Nothing

typeTree :: TypeDecoders -> AbsType -> BTree
typeTree = typeTree_ BFork (\t s bs ps -> BCon ps)

typeTree_ f c e t = conTree_ f c t [] (solve t e)

conTree_ f c t bs (ConTree l r) = f (conTree_ f c t (0:bs) l) (conTree_ f c t (1:bs) r)

-- conTree_ f c e t bs (Con cn cs) = c t (L.toString cn) (reverse bs) (map (typeTree_ f c e) (fieldsTypes cs))
conTree_ f c t bs (Con cn cs) = c t (L.toString cn) (reverse bs) (fieldsTypes cs)

typeDecoder :: TypeDecoders -> AbsType -> Get Val
typeDecoder = typeDecoder_ Val

typeDecoder_ c e t = conDecoder_ c e t [] (solve t e)

-- conDecoder :: TypeDecoders -> [Bool] -> ConTree AbsRef -> Get Val
conDecoder_ c e t bs (ConTree l r) = do
  tag <- getBool
  conDecoder_ c e t (tag:bs) (if tag then r else l)

conDecoder_ c e t bs (Con cn cs) = c t (L.toString cn) (reverse bs) <$> mapM (typeDecoder_ c e) (fieldsTypes cs)


-- Decode a TypedValue without previous knowledge of its type
-- decodeTypedBLOB :: Encoded TypedBLOB -> Decoded Val
-- decodeTypedBLOB enc =
--   case runPartialGet decode (bytes enc) 0 of
--     Left e -> Left e
--     Right (typ,bs,n) -> case runPartialGet (decoderForType typ) bs n of
--       Left e -> Left e
--       -- NOTE: we ignore left overs
--       Right r@(v,bs',n') -> Right v -- partBits bs n

-- -- decodeDebug :: forall a. (Flat a,Model a) => a -> Decoded Val
-- decodeDebug a =
--      let ty = absType (Proxy :: Proxy a)
--          Encoded bs = encoded a
--      in runGetOrFail (decoderForType ty) bs



