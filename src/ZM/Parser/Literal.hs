{-# LANGUAGE OverloadedStrings #-}

module ZM.Parser.Literal (
    Literal (..),
    literal,
) where

import Data.Text
import Text.Megaparsec
import ZM.Parser.Lexer
import ZM.Parser.Types

data Literal -- e
    = LInteger Integer
    | LFloat Double -- Rational
    | LChar Char
    | LString Text
    deriving (Eq, Ord, Show)

{- |
>>> parseMaybe literal "13"
Just (LInteger 13)

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
        [ try (LInteger <$> signed)
        , LFloat <$> float
        , LChar <$> charLiteral
        , LString <$> textLiteral
        ]
