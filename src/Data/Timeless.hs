{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- |A long-term storage/exchange format
-- that supports different meta models
module Data.Timeless (
    Timeless(..),
    timelessHash,
    timelessExplicit,
    timelessSimple,
    untimeless,
    ) where

import qualified Data.ByteString  as B
import           Flat
import           Data.Model
import           ZM.Abs
import           ZM.BLOB

import           ZM.Types

{-
Dynamically recover a value, discover metamodel of a data type, its type

instance Pretty Timeless where
  pPrint (Timeless
-}

-- import           Debug.Trace
traceShowId = id

-- |Value serialised in the timeless format
data Timeless = Timeless { timelessMeta :: TypedBLOB, timelessValue :: BLOB FlatEncoding }
  deriving (Eq, Ord, Show, NFData, Generic, Flat, Model)

-- ex: True :: TypeCon .. :: Type AbsRef
-- ex: True :: BoolType :: SimpleType
timelessHash :: forall a . (Model a, Flat a) => a -> Timeless
timelessHash a = Timeless (typedBLOB (absType (Proxy :: Proxy a))) (blob FlatEncoding . flat $ a)

timelessExplicit :: forall a . (Model a, Flat a) => a -> Timeless
timelessExplicit a = Timeless (typedBLOB (absTypeModel (Proxy :: Proxy a))) (blob FlatEncoding . flat $ a)

timelessSimple :: Bool -> Timeless
timelessSimple a = Timeless (typedBLOB BoolType) (blob FlatEncoding . flat $ a)

untimelessDynamic_ :: Timeless -> TypedDecoded (B.ByteString,AbsType, AbsType)
untimelessDynamic_ (Timeless (TypedBLOB meta tbs) bs) =
  if meta == metaAbsType
    then (unblob bs,,meta) <$> (errMap DecodeError . unflat . unblob $ tbs :: TypedDecoded AbsType)
    else Left $ UnknownMetaModel meta

--untimeless ::  forall a.  (Flat a, Model a) => TypedDecoded Timeless -> TypedDecoded a
--untimeless dt = dt >>= untimeless_

untimeless ::  forall a.  (Flat a, Model a) => Timeless -> TypedDecoded a
untimeless (Timeless (TypedBLOB meta tbs) bs) =
  let expectedType = absType (Proxy :: Proxy a)
  in if meta == metaAbsType
     then let Right (actualType :: AbsType) = unflat . unblob $ tbs
          in if expectedType == actualType
             then errMap DecodeError . unflat . unblob $ bs
             else typeErr expectedType actualType
     else if meta == metaExplicitType
          then let Right (actualTypeModel :: AbsTypeModel) = unflat . unblob $ tbs
                   actualType = typeName actualTypeModel
               in if expectedType == actualType
                  then errMap DecodeError . unflat . unblob $ bs
                  else typeErr expectedType actualType
          else if meta == metaSimpleType
               then let Right (simpleType :: SimpleType) = unflat . unblob $ tbs
                        actualType = if simpleType == BoolType then metaBool else metaChar
                    in if expectedType == actualType
                       then errMap DecodeError . unflat . unblob $ bs
                       else typeErr expectedType actualType -- Left "SimpleType can only be a Boolean or a Char"
               else Left $ UnknownMetaModel meta

-- Fake type system
-- just for demo purposes
data SimpleType = BoolType
                | CharType deriving (Eq, Ord, Show, Generic,Flat,Model)

metaAbsType :: AbsType
metaAbsType = absType (Proxy :: Proxy AbsType)

metaExplicitType :: AbsType
metaExplicitType = absType (Proxy :: Proxy AbsTypeModel)

metaSimpleType :: AbsType
metaSimpleType = absType (Proxy :: Proxy SimpleType)

metaBool :: AbsType
metaBool = absType (Proxy :: Proxy Bool)

metaChar :: AbsType
metaChar = absType (Proxy :: Proxy Char)

fun :: Type t -> Type t
fun ta = let TypeApp f a = ta in f
