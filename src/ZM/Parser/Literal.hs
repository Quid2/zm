{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module ZM.Parser.Literal (
    Literal (..),
    literal,
) where

import Data.Text
import Text.Megaparsec
import ZM.Parser.Lexer
import ZM.Parser.Types
import ZM.Pretty
import Text.PrettyPrint (doubleQuotes)

data Literal -- e
    = LInteger Integer
    | LFloat Double -- Rational
    | LChar Char
    | LString Text
    deriving (Eq, Ord, Show)

instance Pretty Literal where
    pPrint (LInteger i) = pPrint i
    pPrint (LFloat f) = pPrint f
    pPrint (LChar c) = chr c
    pPrint (LString t) = doubleQuotes $ txt t

{- |
>>> p = parseMaybe literal

>>> p "-9e+11"
Just (LFloat (-9.0e11))

>>> parseMaybe literal "13"
Just (LInteger 13)

>>> parseMaybe literal "-13"
Just (LInteger (-13))

>>> parseMaybe literal "13.55"
Just (LFloat 13.55)

>>> parseMaybe literal "'a'"
Just (LChar 'a')

>>> parseMaybe literal "\"abc\""
Just (LString "abc")
-}
literal :: Parser Literal
literal =
    choice
        [ LInteger <$> try signedInt
        , LFloat <$> try signedFloat
        , LChar <$> charLiteral
        , LString <$> textLiteral
        ]
