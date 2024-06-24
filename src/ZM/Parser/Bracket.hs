{-# LANGUAGE OverloadedStrings #-}

module ZM.Parser.Bracket (
    Bracket (..),
    bracket,
) where

import Data.Text (Text, pack)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import ZM.Parser.Lexer
import ZM.Parser.Types

{- List of values between brackets, space separated, with an optional operand/modifier
>>> import ZM.Parser.Lexer

NOTE: not all symbols are accepted:

>>> parseMaybe (bracket signed) "{* 11 22 *}"
Nothing

>>> parseMaybe (bracket signed) "{ %%11 22%%}" == Nothing
True

>>> parseMaybe (bracket signed) "{%%11 22%%}"
Just (Bracket {open = '{', op = Just "%%", values = [11,22]})

>>> parseMaybe (bracket signed) "{|11 22|}"
Just (Bracket {open = '{', op = Just "|", values = [11,22]})

>>> parseMaybe (bracket signed) "{11 22}"
Just (Bracket {open = '{', op = Nothing, values = [11,22]})

>>> parseMaybe (bracket (symbol "a")) "[a a]"
Just (Bracket {open = '[', op = Nothing, values = ["a","a"]})

>>> parseMaybe (bracket (char 'a')) "[]"
Just (Bracket {open = '[', op = Nothing, values = ""})
-}
bracket :: Parser e -> Parser (Bracket e)
bracket pe = do
    (o, c) <- choice (map (\oc@(o, _) -> oc <$ char o) brackets)
    msym <- optional sym
    vs <- many pe
    _ <- maybe (string "") string msym
    _ <- char c
    return $ Bracket o msym vs

brackets :: [(Char, Char)]
brackets = [('{', '}'), ('[', ']')] -- ('<','>'),('«','»')]

data Bracket e = Bracket
    { open :: Char
    , op :: Maybe Text
    , values :: [e]
    }
    deriving (Show, Eq, Ord)
