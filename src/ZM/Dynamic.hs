{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Dynamical decoding of serialised typed values
module ZM.Dynamic
  ( decodeAbsTypeModel
  , typeDecoder
  , typeDecoderMap
  , MapTypeDecoder
  ) where

import qualified Data.ByteString as B
import           Data.Flat
import qualified Data.Map        as M
import           Data.Model
import           ZM.Transform
import           ZM.Types

-- $setup
-- >>> import ZM.Abs
-- >>> import ZM.Pretty
-- >>> import ZM.Pretty.Value
-- >>> import Data.Word
{-| Decode a Flat encoded value with a known type model to the corresponding Value.


>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Bool)) (flat True)
Right (Value {valType = TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)), valName = "True", valBits = [True], valFields = []})

Same but prettier:

>>> prettyShow <$> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Bool)) (flat True)
Right "True"

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy (Maybe Bool))) (flat $ Just True)
Right (Value {valType = TypeApp (TypeCon (AbsRef (SHAKE128_48 218 104 54 119 143 212))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))), valName = "Just", valBits = [True], valFields = [Value {valType = TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)), valName = "True", valBits = [True], valFields = []}]})

>>> prettyShow <$> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Word8)) (flat (11 :: Word8))
Right "11"

If we use the wrong type we get an error:
>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Word8)) (flat 1.1)
Left (TooMuchSpace ...

or not, if the binary sequence happens to have the same length of a value in the wrong type:
>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Word8)) (flat (11::Int))
Right (Value {valType = TypeCon (AbsRef (SHAKE128_48 177 244 106 73 200 248)), valName = "V22", valBits = [False,False,False,True,False,True,True,False], valFields = []})

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Word8)) (flat (11::Word8))
Right (Value {valType = TypeCon (AbsRef (SHAKE128_48 177 244 106 73 200 248)), valName = "V11", valBits = [False,False,False,False,True,False,True,True], valFields = []})

The valBits refer only to the bottom level:
>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Double)) (flat 1.1)
Right (Value {valType = TypeCon (AbsRef (SHAKE128_48 203 169 89 107 70 87)), valName = "IEEE_754_binary64", valBits = [], valFields = [Value {valType = TypeCon (AbsRef (SHAKE128_48 84 159 145 243 176 236)), valName = "Positive", valBits = [False]...

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy Char)) (flat 'a')
Right (Value {valType = TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69)), valName = "Char"...

>>> decodeAbsTypeModel (absTypeModel (Proxy::Proxy String)) (flat "abc")
Right (Value {valType = TypeApp (TypeCon (AbsRef (SHAKE128_48 184 205 19 24 113 152))) (TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69))), valName = "Cons", valBits = [True]...

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

conDecoder ::
     (Convertible name String)
  => MapTypeDecoder
  -> AbsType
  -> [Bool]
  -> ConTree name AbsRef
  -> Get Value
conDecoder env t bs (ConTree l r) = do
  tag :: Bool <- decode
  conDecoder
    env
    t
    (tag : bs)
    (if tag
       then r
       else l)
conDecoder env t bs (Con cn cs) =
  Value t (convert cn) (reverse bs) <$> mapM (`solve` env) (fieldsTypes cs)
