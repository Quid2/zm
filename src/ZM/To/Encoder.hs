{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- avoid warnings for test functions
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- Convert textual ZM values to their canonical binary representation -}
module ZM.To.Encoder (
    encoderFor,
    typeEncoder,
    -- , parseErrorPretty
)
where

import Control.Applicative.Permutations
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Int
import qualified Data.Map as M
import Data.Monoid (Sum (Sum))
import Data.Text (Text, concat, unpack)
import qualified Data.Text as T
import Data.Void
import Data.Word
import Flat.Encoder
import GHC.IO.Encoding (TextDecoder)
import Text.Megaparsec
import ZM
import ZM.Parser.ADT
import ZM.Parser.Lexer (
    symbol,
 )
import ZM.Parser.Literal
import ZM.Parser.Types (Parser, TypeName, asTypeName)
import ZM.Parser.Util (cpars, doc, parenthesis)
import qualified ZM.Type.String as ZM

{- | A direct converter from textual to binary format for ZM values
 type ParserEncoder = String -> Either (ParseError Char Void) ByteString
-}
type ParserEncoder = Text -> Either (ParseErrorBundle Text Void) ByteString

-- | A mapping from an absolute type and its parser
type TypeEncoderMap = M.Map (Type AbsRef) (Parser OBJ)

-- | The result of the parsing of a ZM value: the number of encoded bits and the encoding.
type OBJ = (Sum NumBits, Encoding)

{- $setup
>>> :set -XDeriveGeneric -XDeriveAnyClass -XRankNTypes -XScopedTypeVariables -XNoMonomorphismRestriction
>>> import Data.Bifunctor()
>>> import Flat.Bits()
>>> import Flat.Run()
>>> import ZM.Parser.Util (syntaxError)
>>> newtype Msg1 = Msg1 { b0 :: Bool } deriving (Generic, Model, Flat, Show)
>>> data Msg3 = Msg3 { b1 :: Bool, b2 :: Bool, b3 :: Bool} deriving (Generic, Model, Flat, Show)
>>> data V = V { v0 :: Char , v1 :: String , v4 :: Float, v5 :: Double} deriving (Generic, Model, Flat, Show)
>>> let m1 = Msg1 True
>>> let m3 = Msg3 True False False
>>> let v1 = V 'j' "78金門" 4.4E34 (-9.9E-19)
>>> let enc = \p s ->  bimap  (prettyShow . syntaxError) prettyShow . encoderFor p $ s
-}

-- u = M.keys $ typeEncoderMap $ absTypeModel (Proxy :: Proxy V)
-- e = encodeWith encBool "(True ) "
-- l = shEnc (V 'k' "bb" 4.4 9.9E11)
-- z = encoderFor (Proxy :: Proxy Bool) "True"
-- m = prettyShow $ absTypeModel (Proxy :: Proxy Float)
-- data Msg1 = Msg1 {b0::Bool} deriving (Generic,Model,Flat,Show)
-- instance Pretty Msg1 where pPrint = text . shob
-- instance Pretty Msg3 where pPrint = text . show
-- instance Pretty V where pPrint = text . show

-- ff = flat (4.4 :: Double)
-- n = shEnc $ Msg3 True False True
-- shEnc
--   :: forall a
--    . (Show a, Model a)
--   => a
--   -> Either (ParseError Char Void) ByteString
-- shEnc v = encoderFor (Proxy :: Proxy a) (" " ++ show v ++ " ")

{-
Return a "String to Flat" encoder for values of the indicated type.

>>> enc (Proxy :: Proxy (Maybe Bool)) "Just True"
Right "[193]"

>>> enc (Proxy :: Proxy (Maybe (Bool,Bool))) "Just (Tuple2 True False)"
Right "[193]"

>>> enc (Proxy :: Proxy [Bool]) "Cons True (Cons False Nil)"
Right "[225]"

TODO: support list special syntax

>>> enc (Proxy :: Proxy [Bool]) "[True,False]"
Left "\"unexpected \\\"[Tru\\\" expecting \\\"Cons\\\", \\\"Nil\\\", or '('\"@(0:0)"

>>> enc (Proxy :: Proxy (Maybe Bool)) "(Just (True))"
Right "[193]"

>>> enc (Proxy :: Proxy Char) "'a'"
Right "[97, 1]"

>>> enc (Proxy :: Proxy Float) "-123"
Right "[194, 246, 0, 0, 1]"

>>> enc (Proxy :: Proxy Int8) "+123"
Right "[246, 1]"

>>> enc (Proxy :: Proxy Bool) "True : K306f1981b42c "
Left "\"Type Mismatch: declared type is .K306f1981b42c expected type is K306f1981b41c\"@(0:21)"

>>> enc (Proxy :: Proxy Bool) "True : K306f1981b41c "
Right "[129]"

>>> enc (Proxy :: Proxy Bool) "True:Bool"
Left "\"unexpected 'B' expecting '(', '.', 'K', or lowercase letter\"@(0:5)"

>>> enc (Proxy :: Proxy Bool) "True:Bool.K306f1981b41c "
Left "\"unexpected 'B' expecting '(', '.', 'K', or lowercase letter\"@(0:5)"

Some types use a special syntax (Float, Double, Char, String, Integer):

>>> enc (Proxy :: Proxy Float) "3.1416:Int.Kb53bec846608"
Left "\"unexpected 'I' expecting '(', '.', 'K', or lowercase letter\"@(0:7)"

-- BAD: we do not check the type name

>>> enc (Proxy :: Proxy Float) "3.1416:Float.Kb53bec846608"
Left "\"unexpected 'F' expecting '(', '.', 'K', or lowercase letter\"@(0:7)"

-- >>> enc (Proxy :: Proxy Double) "3.1416:Int.Kb53bec846608"
-- Right "[64, 73, 15, 249, 1]"

>>> enc (Proxy :: Proxy Char) "'v'"
Right "[118, 1]"

>>> enc (Proxy :: Proxy String) "\"ab\n金\nc\""
Right "[176, 216, 161, 93, 26, 48, 40, 85, 141]"

>>> enc (Proxy :: Proxy (Maybe Bool)) "Just True:Int"
Left "\"unexpected 'I' expecting '(', '.', 'K', or lowercase letter\"@(0:10)"

>>> enc (Proxy :: Proxy (Maybe Bool)) "Just True:Maybe (Bool.K306f1981b41c)"
Left "\"unexpected 'M' expecting '(', '.', 'K', or lowercase letter\"@(0:10)"

-- >>> enc (Proxy :: Proxy (Maybe Bool)) "Just (True:Bool.K306f1981b41c)"
Right "[193]"

-- >>> enc (Proxy :: Proxy (Either (Maybe Bool) Int)) "Left (Just True): Either.K306f1981b41c (Maybe Bool.K306f1981b41c) Word8.K306f1981b41c"
-- >>> enc (Proxy :: Proxy (Either (Maybe Bool) Int)) "Left (Just True): Either (Maybe Bool.K306f1981b41c) Int"
Right "a"

>>> encodeOK "ABC"
True

>>> encodeOK False
True

>>> encodeOK m1
True

>>> encodeOK2 m1 "Msg1 (True)"
True

>>> encodeOK2 m1 "(Msg1 {  b0 =(True) })"
True

>>> encodeOK m3
True

prop> \(x::Char) -> encodeOK x
Add QuickCheck to your cabal dependencies to run this test.

\$undisplayed
>>> encodeOK '金'
True

-- prop> \(x::String) -> encodeOK x

>>> encodeOK "!金金?"
True

{\-
prop> \(x::Float) -> encodeOK x
Add QuickCheck to your cabal dependencies to run this test.

-- prop> \(x::Double) -> encodeOK x

-- prop> \(x::Integer) -> encodeOK x

-- prop> \(v::Int) -> encodeOK2 v (show v)

-- >>> encodeOK2 m3 "Msg3 (True) False  False"
-- >>> encodeOK2 m3 "Msg3 {b3=False,   b1=True,b2=False}"
-- >>> encodeOK2 m3 "Msg3 {b3=False b1=True b2=False}"
-- >>> encodeOK '金'
-- >>> encodeOK "abc金"
-- >>> encodeOK2 "ab"             "\"ab\""
-- >>> encodeOK2 (-3.4 :: Float)  "-3.4"
-- >>> encodeOK2 (-11 :: Float)   "-11"
-- >>> encodeOK2 (11 :: Float)    "11"
-- >>> encodeOK2 (11 :: Float)    "+11"
-- >>> encodeOK2 (-11.1 :: Float) "-11.1"
-- >>> encodeOK (4.4 :: Float)
-- >>> encodeOK (-4.4 :: Double)
-- >>> encodeOK2 v1 "V 'j' (\"78金門\") 4.4E34 ((-9.9E-19))"
-- >>> encodeOK v1
-}

-- Check that the binary representation of a value is equal to the one produced by the encoding parser.
encodeOK :: forall a. (Flat a, Model a, Show a) => a -> Bool
encodeOK v = encodeOK2 v (T.pack $ show v) -- (prettyShow v)

encodeOK2 :: forall a. (Flat a, Model a) => a -> Text -> Bool
encodeOK2 v s = either (const False) (== flat v) $ encoderFor (Proxy :: Proxy a) (T.concat [" ", s, " "])

-- | Return a Flat encoder for values of the ZM type corresponding to the provided Haskell type
encoderFor :: (Model a) => Proxy a -> ParserEncoder
encoderFor = typeEncoder . absTypeModel

-- | Return a Flat encoder for values of the provided ZM type
typeEncoder :: AbsTypeModel -> ParserEncoder
typeEncoder tm =
    encodeWith $ doc (typeEncoder_ (typeEncoderMap tm) (typeName tm))

encodeWith :: Parser OBJ -> ParserEncoder
encodeWith pe s = flat <$> parse pe "" s

-- 'Fake' instance used to simplify the creation of the final binary representation
instance Flat OBJ where
    decode = undefined
    encode (_, enc) = enc
    size (Sum s, _) n = n + s

typeEncoderMap :: AbsTypeModel -> TypeEncoderMap
typeEncoderMap tm =
    let denv =
            addSpecial (Proxy :: Proxy Float) signedFloat
                . addSpecial (Proxy :: Proxy Double) signedFloat
                . addSpecial (Proxy :: Proxy Word8) unsigned
                . addSpecial (Proxy :: Proxy Word16) unsigned
                . addSpecial (Proxy :: Proxy Word32) unsigned
                . addSpecial (Proxy :: Proxy Word64) unsigned
                . addSpecial (Proxy :: Proxy Int8) signedInt
                . addSpecial (Proxy :: Proxy Int16) signedInt
                . addSpecial (Proxy :: Proxy Int32) signedInt
                . addSpecial (Proxy :: Proxy Int64) signedInt
                . addSpecial (Proxy :: Proxy Char) charLiteral
                . addSpecial (Proxy :: Proxy ZM.String) (ZM.String <$> singleLineText)
                . addSpecial (Proxy :: Proxy [Char]) singleLineText
                $ M.mapWithKey
                    (\t ct -> conEnc denv t (Sum 0) mempty ct)
                    (typeTree tm)
     in denv

-- | Add a custom parser for a ZM type
addSpecial ::
    (Model a, Flat a) =>
    Proxy a ->
    Parser a ->
    TypeEncoderMap ->
    TypeEncoderMap
addSpecial proxy parser env =
    let tm = absTypeModel proxy in M.insert (typeName tm) (encParser parser) env

conEnc ::
    (Convertible a Text) =>
    TypeEncoderMap ->
    t ->
    Sum NumBits ->
    Encoding ->
    ConTree a AbsRef ->
    Parser OBJ
conEnc env t numBits enc ct =
    parenthesis (conEnc env t numBits enc ct) <|> conEncoder env t numBits enc ct

conEncoder ::
    (Convertible a Text) =>
    TypeEncoderMap ->
    t ->
    Sum NumBits ->
    Encoding ->
    ConTree a AbsRef ->
    Parser OBJ
conEncoder env t numBits enc (ConTree l r) =
    let numBits' = (+ 1) <$> numBits
     in conEncoder env t numBits' (enc <> eFalse) l
            <|> conEncoder env t numBits' (enc <> eTrue) r
conEncoder env _ numBits enc (Con cn (Left ps)) =
    constrEnc (convert cn) numBits enc <> flds env ps
conEncoder env _ numBits enc (Con cn (Right ps)) =
    constrEnc (convert cn) numBits enc <> mnamedFlds env ps
  where
    mnamedFlds env ps =
        cpars (namedFlds (map (bimap convert (typeEncoder_ env)) ps))
            <|> flds env (map snd ps)

flds :: TypeEncoderMap -> [AbsType] -> Parser OBJ
flds env ps = mconcat (map (typeEncoder_ env) ps)

constrEnc :: Text -> Sum NumBits -> Encoding -> Parser OBJ
constrEnc name numBits enc = (numBits, enc) <$ symbol name

{- | Return an encoder for a given type
 typeEncoder :: AbsTypeModel -> Parser OBJ
 typeEncoder env tm = solve (t (typeEncoderMap tm)
-}
typeEncoder_ :: TypeEncoderMap -> AbsType -> Parser OBJ
typeEncoder_ env typ = do
    let typeParser :: Parser (OBJ, Type (TypeName Identifier)) =
            (undefined,) <$> (symbol ":" *> parType namedOrAbsRef)
        valueParser = (,undefined) <$> solve typ env
    ((r, _), mtp) <- (,) <$> valueParser <*> optional typeParser
    case mtp of
        Nothing -> return r
        -- BUG: no name check
        Just (_, dtyp) ->
            if dtyp == (asTypeName Nothing . Just <$> typ)
                then return r
                else
                    fail $
                        unwords
                            [ "Type Mismatch: declared type is"
                            , prettyShow dtyp
                            , "expected type is"
                            , prettyShow typ
                            ]

{-
NOT DEFINED ELSEWHERE? see ZM.Parser.Value for declarative form

Parser for records fields.

For example, for a type such as :

data Msg = Msg {sent::Bool,content::String}

it would parse:

sent=Bool,content="some message"
-}
namedFlds :: [(Text, Parser OBJ)] -> Parser OBJ
namedFlds =
    (mconcat <$>)
        . intercalateEffect (optional (symbol ","))
        . traverse
            (toPermutation . namedFld)

namedFld :: (Text, Parser OBJ) -> Parser OBJ
namedFld (name, parser) = do
    _ <- symbol name
    _ <- symbol "="
    parser

signedFloatEnc :: Parser OBJ
signedFloatEnc = encParser (signedFloat :: Parser Float)

-- | Transform a parser in an encoding parser
encParser :: (Flat a) => Parser a -> Parser OBJ
encParser p = (\s -> (Sum $ getSize s, encode s)) <$> par p
  where
    par p = parenthesis (par p) <|> p

-- dtBool = simpleConstr "False" <|> simpleConstr "True"
-- simpleConstr :: String -> Parser Value
-- simpleConstr name =
--   pars (simpleConstr name) <|> Value <$> symbol name <*> pure (Left [])
