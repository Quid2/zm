module ZM.Parser.Bracket where

import ZM.Parser.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text(Text,pack)
import ZM.Parser.Lexer
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

{- List of values between brackets, space separated, with an optional operand/modifier
>>> import ZM.Parser.Lexer

>>> parseMaybe (bracket signed) "{ %%11 22%%}"
Nothing

>>> parseMaybe (bracket signed) "{* 11 22 *}"
Nothing

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
    (o,c) <- choice (map (\oc@(o,_) -> oc <$ char o ) brackets)
    msym <- optional sym
    vs <- many pe
    _ <- dbg "close op" $ maybe (string "") string msym
    _ <- char c
    return $ Bracket o (pack <$> msym) vs

brackets :: [(Char, Char)]
brackets = [('{','}'),('[',']')] -- ,('«','»')]

data Bracket e = Bracket {
    open::Char
    ,op::Maybe Text
    ,values::[e]
    } deriving (Show,Eq,Ord)

