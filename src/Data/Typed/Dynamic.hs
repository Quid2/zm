{-# LANGUAGE ScopedTypeVariables #-}
-- |Decoding serialised values with a known type model
module Data.Typed.Dynamic(
  MapTypeDecoder
  ,typeDecoder
  ,typeDecoderMap
  ,decodeAbsTypeModel
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
decodeAbsTypeModel tm = unflatWith (typeDecoder tm)

typeDecoder :: AbsTypeModel -> Get Value
typeDecoder tm = solve (typeName tm) (typeDecoderMap tm)

type MapTypeDecoder = M.Map (Type AbsRef) (Get Value)

typeDecoderMap :: AbsTypeModel -> MapTypeDecoder
typeDecoderMap tm =
  let denv = M.mapWithKey (\t ct -> conDecoder denv t [] ct) (typeTree tm)
  in denv

conDecoder :: (L.StringLike name) => MapTypeDecoder -> AbsType -> [Bool] -> ConTree name AbsRef -> Get Value
conDecoder env t bs (ConTree l r) = do
  tag <- getBool
  conDecoder env t (tag:bs) (if tag then r else l)

conDecoder env t bs (Con cn cs) = Value t (L.toString cn) (reverse bs) <$> mapM (`solve` env) (fieldsTypes cs)


