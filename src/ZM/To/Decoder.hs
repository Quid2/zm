
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses ,PatternSynonyms  #-}

-- |Dynamical decoding of serialised typed values
module ZM.To.Decoder
  ( decodeAbsTypeModel
  , typeDecoder
  , typeDecoderMap
  )
where

import qualified Data.ByteString               as B
import           Flat
import qualified Data.Map                      as M
import           Data.Model
import           ZM.Transform
import           ZM.Types                       ( AbsTypeModel
                                                , AbsRef
                                                , AbsType
                                                , Identifier
                                                )
import           ZM.Parser.Types                ( Value
                                                , pattern Value
                                                )
import           Flat.Decoder.Types

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import ZM
-- >>> import ZM.Abs
-- >>> import ZM.Pretty
-- >>> import Data.Word
-- >>> import Data.Int
-- >>> import ZM.Types
-- >>> import ZM.Parser.Types  

{-| Decode a Flat encoded value with a known type model to the corresponding Value.

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Bool)) (flat True) == Right (Value {valType = TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)), valName = "True", valBits = [True], valFields = []})
True

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy (Maybe Bool))) (flat $ Just True) == Right (Value {valType = TypeApp (TypeCon (AbsRef (SHAKE128_48 218 104 54 119 143 212))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))), valName = "Just", valBits = [True], valFields = [Value {valType = TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)), valName = "True", valBits = [True], valFields = []}]})
True

If we use the wrong type we get an error:

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Word8)) (flat 1.1)
Left (TooMuchSpace ...

Or not, if the binary sequence happens to have the same length of a value of the wrong type:

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Word8)) (flat (11::Int)) == Right (Value {valType = TypeCon (AbsRef (SHAKE128_48 177 244 106 73 200 248)), valName = "V22", valBits = [False,False,False,True,False,True,True,False], valFields = []})
True

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Word8)) (flat (11::Word8)) == Right (Value {valType = TypeCon (AbsRef (SHAKE128_48 177 244 106 73 200 248)), valName = "V11", valBits = [False,False,False,False,True,False,True,True], valFields = []})
True

The valBits refer only to the bottom level:

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy (Maybe Bool))) (flat $ Just False)
Right (Annotate (TypeApp (TypeCon (AbsRef (SHAKE128_48 218 104 54 119 143 212))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))),[True]) (ConstrF "Just" (Left [Annotate (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)),[False]) (ConstrF "False" (Left []))])))

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Char)) (flat 'a')
Right (Annotate (TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69)),[]) (ConstrF "Char" (Left [Annotate (TypeCon (AbsRef (SHAKE128_48 36 18 121 156 153 241)),[]) (ConstrF "Word32" (Left [Annotate (TypeCon (AbsRef (SHAKE128_48 249 46 131 57 144 138)),[]) (ConstrF "Word" (Left [Annotate (TypeApp (TypeCon (AbsRef (SHAKE128_48 32 255 172 200 248 201))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))))),[]) (ConstrF "LeastSignificantFirst" (Left [Annotate (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126)))),[False]) (ConstrF "Elem" (Left [Annotate (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))),[]) (ConstrF "MostSignificantFirst" (Left [Annotate (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126)),[True,True,False,False,False,False,True]) (ConstrF "V97" (Left []))]))]))]))]))]))])))

>>> let Right (Value {valType = TypeApp (TypeCon (AbsRef (SHAKE128_48 184 205 19 24 113 152))) (TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69))), valName = "Cons", valBits = [True] , valFields=_}) = decodeAbsTypeModel (absTypeModel (Proxy::Proxy String)) (flat "abc") in True
True

TODO: implement pretty
prettyShow <$> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Bool)) (flat True)
Right "True"

prettyShow <$> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Word8)) (flat (11 :: Word8))
Right "11"
-}


decodeAbsTypeModel :: AbsTypeModel -> B.ByteString -> Decoded Value
decodeAbsTypeModel = unflatWith . typeDecoder

-- |Returns a decoder for the type defined by the given model
typeDecoder :: AbsTypeModel -> Get Value
typeDecoder = typeOp typeDecoderMap

-- |A mapping between references to absolute types and the corresponding decoder
type TypeDecoderMap = TypeMap (Get Value)

-- |Returns decoders for all types in the given model
typeDecoderMap :: AbsTypeModel -> TypeDecoderMap
typeDecoderMap = typeOpMap (conDecoder [])

conDecoder
  :: (Convertible name String)
  => [Bool]
  -> TypeDecoderMap
  -> AbsType
  -> ConTree name AbsRef
  -> Get Value
conDecoder bs env t (ConTree l r) = do
  tag :: Bool <- decode
  conDecoder (tag : bs) env t (if tag then r else l)

conDecoder bs env t (Con cn cs) =
  Value t (convert cn) (reverse bs) <$> mapM (`solve` env) (fieldsTypes cs)


typeOp :: (AbsTypeModel -> TypeMap r) -> AbsTypeModel -> r
typeOp opMap tm = solve (typeName tm) (opMap tm)

-- |A mapping between references to absolute types and the corresponding operation
type TypeMap r = M.Map (Type AbsRef) r

typeOpMap
  :: (TypeMap r -> AbsType -> ConTree Identifier AbsRef -> r)
  -> AbsTypeModel
  -> TypeMap r
typeOpMap op tm =
  let denv = M.mapWithKey (\t ct -> op denv t ct) (typeTree tm) in denv
