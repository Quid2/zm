{-# LANGUAGE ScopedTypeVariables #-}

-- |Dynamical decoding of serialised values
module Data.Typed.Dynamic(
  decodeAbsTypeModel
  ,typeDecoder
  ,MapTypeDecoder
  ,typeDecoderMap
  ) where

import qualified Data.ByteString.Lazy      as L
import           Data.Flat
import qualified Data.ListLike.String      as L
import qualified Data.Map                  as M
import           Data.Model
import           Data.Typed.Transform
import           Data.Typed.Types
import           Data.Typed.Value

-- | Decode a flat encoded value with a known type model to the corresponding Value
decodeAbsTypeModel :: AbsTypeModel -> L.ByteString -> Decoded Value
decodeAbsTypeModel = unflatWith . postAlignedDecoder . typeDecoder

-- |Returns decoder for the given model
typeDecoder :: AbsTypeModel -> Get Value
typeDecoder tm = solve (typeName tm) (typeDecoderMap tm)

-- |A mapping between references to absolute types and the corresponding decoder
type MapTypeDecoder = M.Map (Type AbsRef) (Get Value)

-- |Returns decoders mapping for the given model
typeDecoderMap :: AbsTypeModel -> MapTypeDecoder
typeDecoderMap tm =
  let denv = M.mapWithKey (\t ct -> conDecoder denv t [] ct) (typeTree tm)
  in denv

conDecoder :: (L.StringLike name) => MapTypeDecoder -> AbsType -> [Bool] -> ConTree name AbsRef -> Get Value
conDecoder env t bs (ConTree l r) = do
  tag :: Bool <- decode
  conDecoder env t (tag:bs) (if tag then r else l)

conDecoder env t bs (Con cn cs) = Value t (L.toString cn) (reverse bs) <$> mapM (`solve` env) (fieldsTypes cs)


