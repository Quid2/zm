{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Data.Typed.BLOB (
    typedBLOB,
    typedBLOB_,
    untypedBLOB,
    typedValue,
    untypedValue,
    typeErr,
    module Data.BLOB,
    ) where

import           Data.BLOB
import           Data.Flat
import           Data.Model
import           Data.Typed.Class
import           Data.Typed.Pretty ()
import           Data.Typed.Types
import           Data.Typed.Util
import qualified Data.ByteString as B

-- WARN: adds additional end alignment byte
typedBLOB :: forall a . (Model a,Flat a) => a -> TypedBLOB
typedBLOB = typedBLOB_ (absType (Proxy :: Proxy a))

typedBLOB_ :: Flat r => AbsType -> r -> TypedBLOB
typedBLOB_ t v = TypedBLOB t (blob FlatEncoding . flatStrict $ v)

typedValue :: forall a . Model a => a -> TypedValue a
typedValue = TypedValue (absType (Proxy :: Proxy a))

untypedBLOB ::  forall a.  (Flat a, Model a) => Decoded TypedBLOB -> TypedDecoded a
untypedBLOB ea = case ea of
                    Left e -> Left . DecodeError $ e
                    Right (TypedBLOB typ' bs) ->
                      let typ = absType (Proxy :: Proxy a)
                      in if (typ' /= typ)
                         then typeErr typ typ'
                         else errMap DecodeError . unflat $ (unblob bs :: B.ByteString)

untypedValue ::  (Flat a, Model a) => Decoded (TypedValue a) -> TypedDecoded a
untypedValue ea = case ea of
                    Left e -> Left . DecodeError $ e
                    Right (TypedValue typ' a) ->
                      let typ = absType (proxyOf a)
                      in if typ' /= typ
                         then typeErr typ typ'
                         else Right a

typeErr typ typ' = Left $ WrongType typ typ'

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



