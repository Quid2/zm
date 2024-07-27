{-# LANGUAGE OverloadedStrings #-}

module ZM.Parser.Op (
    prefixOp,
    infixOp,
) where

import Data.Char as C
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import ZM.Parser.Lexer
import ZM.Parser.Types

-- TODO: test that the different syntactical classes (infix,prefix,wild) are mutually exclusive

{- |
Parse a ZM prefixOp (a unicode letter followed by zero or more unicode alphanumeric characters or '_')

>>> let p = parseMaybe prefixOp

>>> p "*"
Nothing

>>> p "1"
Nothing

>>> p "A"
Nothing

NOTE: no support for non-ascii characters

>>> p "Gold金en"
Nothing

>>> p "是不是"
Nothing

>>> p "Bool -- a bool"
Nothing

>>> p "ant_13_"
Just "ant_13_"

>>> p "abc12"
Just "abc12"

>>> p "abc1*"
Nothing

>>> Nothing == p "True"
True

>>> Nothing == p "_"
True
-}

-- TODO: add (+) `add`

prefixOp :: Parser Text
prefixOp = lexeme var

var :: Parser Text
var = T.cons <$> lowerChar <*> takeWhileP (Just "alpha numeric or _") (\c -> isAlphaNum c || c == '_')

{-
>>> parseMaybe infixOp "++"
Just "++"

>>> parseMaybe infixOp "_"
Just "_"

>>> parseMaybe infixOp "_xyz"
Nothing

>>> parseMaybe (infixOp >> char '}') "*}"
Just '}'
-}
infixOp :: Parser Text
infixOp = lexeme sym

-- wild = char '_' >> return PWild
