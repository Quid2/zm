{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typed.Value(typedBytes,typedValue,decodeTypedValue,TypeDecoders,typeDecoderEnv,typeDecoder) where

import           Control.Monad.Trans.State
import           Data.Binary.Bits.Get      (Get, getBool, runGet, runPartialGet)
import qualified Data.ByteString.Lazy      as L
import           Data.Flat
import           Data.Foldable
import qualified Data.Map                  as M
import           Data.Model
import           Data.Typed.Class
import           Data.Typed.Instances
import           Data.Typed.Transform
import           Data.Typed.Types
import           Debug.Trace

instance Flat TypedBytes
instance Flat a => Flat (TypedValue a)

typedBytes :: forall a . (Typed a,Flat a) => a -> TypedBytes
typedBytes a = TypedBytes (absType (Proxy :: Proxy a)) (L.unpack . flat $ a)

typedValue :: forall a . Typed a => a -> TypedValue a
typedValue = TypedValue (absType (Proxy :: Proxy a))

--encodedTyped :: (HasModel a ,Binary a) => a -> Encoded
--encodedTyped = encoded . typedValue

-- |Decode making sure that the type is correct
-- decodeTypedValue :: forall m a . (Monad m, Typed a, Flat a) => Encoded (TypedValue a) -> m (Either DeserializeFailure a)
decodeTypedValue bs =
  case decoded bs of
    Left e -> return $ Left e
    Right (TypedValue typ' a) -> do
      let typ = absType (proxyOf a)
      if (typ' /= typ)
        then do
          let rt  = show typ -- simpleTypeS typ
              rt' = show typ' -- simpleTypeS typ'
              msg = unwords $ ["Was expecting type:\n",rt,"\n\nBut the data has type:\n",rt']
              -- putStrLn msg
          return . Left $ msg
        else return . Right $ a

-- Dynamic decoding
x :: Val
x =
   let (absType,absEnv) = absTypeEnv (Proxy::Proxy (Bool,Bool,Bool))
       decEnv = typeDecoderEnv (traceShowId absEnv) (traceShowId absType)
       dec = typeDecoder decEnv absType
       v = runGet dec (L.pack [128+32])
   in v

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

typeDecoder
  :: (Ord t, Show t) =>
     M.Map (Type t) (ConTree t)
     -> Type t -> Get Val
typeDecoder e t = conDecoder e [] (solve t e)

conDecoder e bs (ConTree l r) = do
  tag <- getBool
  conDecoder e (tag:bs) (if tag then r else l)

conDecoder e bs (Con cn cs) = Val cn (reverse bs) <$> mapM (typeDecoder e) (fieldsTypes cs)


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



