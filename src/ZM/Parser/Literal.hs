{-# LANGUAGE OverloadedStrings #-}

module ZM.Parser.Literal (
    Literal (..),
    literal,
) where

import Data.Text
import Text.Megaparsec
import ZM.Parser.Lexer
import ZM.Parser.Types
import ZM.Pretty

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
    pPrint (LString t) = txt t

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
        [ try (LInteger <$> signedInt)
        , LFloat <$> signedFloat
        , LChar <$> charLiteral
        , LString <$> textLiteral
        ]
