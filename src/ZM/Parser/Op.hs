{-# LANGUAGE OverloadedStrings #-}

module ZM.Parser.Op (
    prefixOp,
    infixOp,
) where

import Data.Char as C
import Data.Text (Text, pack)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import ZM.Parser.Lexer
import ZM.Parser.Types

{- |
Parse a ZM prefixOp (a unicode letter followed by zero or more unicode alphanumeric characters or '_')

>>> parseMaybe prefixOp "*"
Nothing

>>> parseMaybe prefixOp "1"
Nothing

>>> parseMaybe prefixOp "A"
Just "A"

>>> parseMaybe prefixOp "Gold金en"
Just "Gold\37329en"

>>> parseMaybe prefixOp "是不是"
Just "\26159\19981\26159"

>>> parseMaybe prefixOp "Bool -- a bool"
Just "Bool"

>>> parseMaybe prefixOp "ant_13_"
Just "ant_13_"
-}

-- TODO: add (+) `add`

prefixOp :: Parser Text
prefixOp = lexeme name

{-
>>> parseMaybe (infixOp >> char '}') "*}"
Just '}'
-}
infixOp :: Parser Text
infixOp = lexeme sym

{-

FIX THIS?

>>> parseMaybe name "_asd"
Nothing

>>> parseMaybe name "abc12"
Just "abc12"

>>> parseMaybe name "abc1*"
Nothing

>>> Nothing == parseMaybe name "True"
True

>>> Nothing == parseMaybe name "_"
True
-}
name :: Parser Text
name = T.cons <$> lowerChar <*> takeWhileP (Just "alpha numeric or _") (\c -> isAlphaNum c || c == '_')
