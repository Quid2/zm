{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Dynamical decoding of serialised typed values
module ZM.Dynamic(
  decodeAbsTypeModel
  ,typeDecoder
  ,typeDecoderMap
  ,MapTypeDecoder
  ) where

import qualified Data.ByteString      as B
import           Data.Flat
import qualified Data.Map             as M
import           Data.Model
import           ZM.Transform
import           ZM.Types

-- $setup
-- >>> import ZM.Abs
-- >>> import ZM.Pretty
-- >>> import ZM.Pretty.Value

{-| Decode a Flat encoded value with a known type model to the corresponding Value.

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Bool)) (flat True)
Right (Value {valType = TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)), valName = "True", valBits = [True], valFields = []})

Same but prettier:

>>> prettyShow <$> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Bool)) (flat True)
Right "True"
-}
decodeAbsTypeModel :: AbsTypeModel -> B.ByteString -> Decoded Value
decodeAbsTypeModel = unflatWith . typeDecoder

-- |Returns a decoder for the type defined by the given model
typeDecoder :: AbsTypeModel -> Get Value
typeDecoder tm = solve (typeName tm) (typeDecoderMap tm)

-- |A mapping between references to absolute types and the corresponding decoder
type MapTypeDecoder = M.Map (Type AbsRef) (Get Value)

-- |Returns decoders for all types in the given model
typeDecoderMap :: AbsTypeModel -> MapTypeDecoder
typeDecoderMap tm =
  let denv = M.mapWithKey (\t ct -> conDecoder denv t [] ct) (typeTree tm)
  in denv

conDecoder :: (Convertible name String) => MapTypeDecoder -> AbsType -> [Bool] -> ConTree name AbsRef -> Get Value
conDecoder env t bs (ConTree l r) = do
  tag :: Bool <- decode
  conDecoder env t (tag:bs) (if tag then r else l)

conDecoder env t bs (Con cn cs) = Value t (convert cn) (reverse bs) <$> mapM (`solve` env) (fieldsTypes cs)


