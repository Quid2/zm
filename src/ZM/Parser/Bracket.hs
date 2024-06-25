{-# LANGUAGE OverloadedStrings #-}

module ZM.Parser.Bracket (
    Bracket (..),
    bracket,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import ZM.Parser.Lexer
import ZM.Parser.Types
import ZM.Pretty

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
bracket pe = lexeme $ do
    (o, c) <- choice (map (\oc@(o, _) -> oc <$ char o) brackets)
    msym <- optional sym
    vs <- many pe
    _ <- maybe (string "") string msym
    _ <- char c
    return $ Bracket o c msym vs

brackets :: [(Char, Char)]
brackets = [('{', '}'), ('[', ']')] -- ('<','>'),('«','»')]

data Bracket e = Bracket
    { open, close :: Char
    , op :: Maybe Text
    , values :: [e]
    }
    deriving (Show, Eq, Ord)

{-
>>> prettyShow <$> parseMaybe (bracket signedInt) "{%%11 22%%}"
Just "{%%11 22%%}"
-}
instance (Pretty e) => Pretty (Bracket e) where
    pPrint (Bracket open close mop vs) =
        let op = txt . fromMaybe "" $ mop
         in chr open <> op <> hsep (map pPrint vs) <> op <> chr close
