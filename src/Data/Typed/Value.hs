{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typed.Value(typedBytes,untypedBytes
                       ,typedValue,untypedValue
                       ,TypeDecoders,typeDecoderEnv,typeDecoder,decodeAbsType) where

import           Control.Monad.Trans.State
import           Data.Binary.Bits.Get      (Get, getBool, runGet, runPartialGet)
import qualified Data.ByteString.Lazy      as L
import           Data.Flat
import           Data.Foldable
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

b :: Decoded (Bool,Bool,Bool)
b = untypedBytes . unflat . flat . typedBytes $ (True,False,True)

instance Flat TypedBytes
-- instance Model TypedBytes
instance Flat a => Flat (TypedValue a)
instance Model a => Model (TypedValue a)
-- instance Model Bytes

typedBytes :: forall a . (Typed a,Flat a) => a -> TypedBytes
typedBytes v = TypedBytes (absType (Proxy :: Proxy a)) (blob FlatEncoding . flat $ v)

typedValue :: forall a . Typed a => a -> TypedValue a
typedValue = TypedValue (absType (Proxy :: Proxy a))

untypedBytes ::  forall a.  (Flat a, Model a) => Either DeserializeFailure TypedBytes -> Either DeserializeFailure a
untypedBytes ea = case ea of
                    Left e -> Left e
                    Right (TypedBytes typ' bs) ->
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

p = mapM_ putStrLn [tst (),tst(False,'x'),tst True,tst 'j',tst "",tst "abc",tst (123::Word8),tst (12345::Word16),tst (123456::Word32),tst (123456789::Word64),tst (123456::Word)]

tst :: forall a . (Model a,Flat a) => a -> String
tst v = prettyShow $ decodeAbsType (absoluteType (Proxy::Proxy a)) (flat v)

-- CHECK: ignores left over bits
decodeAbsType :: AbsoluteType -> L.ByteString -> Val
decodeAbsType at =
  let
    decEnv = typeDecoderEnv (traceShowId $ canonicalEnv at) (traceShowId $ canonicalType at)
    dec = typeDecoder decEnv (canonicalType at)
    in runGet dec

-- A table of decoders, one for every relevant type
type TypeDecoders = M.Map (Type AbsRef) (ConTree AbsRef)
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

typeDecoder :: TypeDecoders -> AbsType -> Get Val
typeDecoder e t = conDecoder e t [] (solve t e)

-- conDecoder :: TypeDecoders -> [Bool] -> ConTree AbsRef -> Get Val
conDecoder e t bs (ConTree l r) = do
  tag <- getBool
  conDecoder e t (tag:bs) (if tag then r else l)

conDecoder e t bs (Con cn cs) = Val t cn (reverse bs) <$> mapM (typeDecoder e) (fieldsTypes cs)


-- Decode a TypedValue without previous knowledge of its type
-- decodeTypedBytes :: Encoded TypedBytes -> Decoded Val
-- decodeTypedBytes enc =
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



