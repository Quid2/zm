{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Data.Typed.BLOB(typedBLOB
                      ,typedBLOB_
                      ,untypedBLOB
                      ,typedValue,untypedValue
                      ,typeErr
                      ) where

import           Data.Flat
import           Data.Model
import           Data.Typed.Class
import           Data.Typed.Pretty ()
import           Data.Typed.Types
import           Data.Typed.Util

-- WARN: adds additional end alignment byte
typedBLOB :: forall a . (Typed a,Flat a) => a -> TypedBLOB
typedBLOB = typedBLOB_ (absType (Proxy :: Proxy a))

typedBLOB_ t v = TypedBLOB t (blob FlatEncoding . flat $ v)

typedValue :: forall a . Typed a => a -> TypedValue a
typedValue = TypedValue (absType (Proxy :: Proxy a))


untypedBLOB ::  forall a.  (Flat a, Model a) => Either DeserializeFailure TypedBLOB -> Either DeserializeFailure a
untypedBLOB ea = case ea of
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



