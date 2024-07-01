{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}

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
import Control.Monad (void)
import Text.PrettyPrint(vcat)

{- List of values between brackets, space separated, with an optional operand/modifier
>>> import ZM.Parser.Lexer

>>> p = parseMaybe (bracket signedInt)

>>> p "{11 {3 4} 22}"
Nothing

>>> p "{11\n 22}"
Just (Bracket {open = '{', close = '}', op = Nothing, values = [11,22]})

NOTE: not all symbols are accepted:

>>> p "{* 11 22 *}"
Just (Bracket {open = '{', close = '}', op = Just "*", values = [11,22]})

>>> p "{ %%11 22%%}" == Nothing
True

>>> p "{%%11 22%%}"
Just (Bracket {open = '{', close = '}', op = Just "%%", values = [11,22]})

>>> p "{|11 22|}"
Just (Bracket {open = '{', close = '}', op = Just "|", values = [11,22]})

>>> p "{11 22}"
Just (Bracket {open = '{', close = '}', op = Nothing, values = [11,22]})

>>> parseMaybe (bracket (symbol "a")) "[a a]"
Just (Bracket {open = '[', close = ']', op = Nothing, values = ["a","a"]})

>>> parseMaybe (bracket (char 'a')) "[]"
Just (Bracket {open = '[', close = ']', op = Nothing, values = ""})

>>> p "[ 1 , 2 ]"
Nothing

>>> p "[,1]"
Nothing

>>> p "[1,\n]"
Nothing

>>> p "[1\n  2,3\n4 , 5\n]"
Nothing

>>> p "[\n1,2,3]"
Nothing

>>> p "[1,2,3]"
Nothing

>>> p "[ 1 \n2 \n\n]"
Just (Bracket {open = '[', close = ']', op = Nothing, values = [1,2]})

>>> p "[1 2]"
Just (Bracket {open = '[', close = ']', op = Nothing, values = [1,2]})

>>> p "[   1\n\n  2\n\n] "
Just (Bracket {open = '[', close = ']', op = Nothing, values = [1,2]})
-}
-- generate test cases 
-- gen = [ T.concat ["[","]"] | n<-[0..2] ,l <- [0..2],sp <- T.take n (T.replicate " ")

bracket :: Parser e -> Parser (Bracket e)
bracket pe = lexeme $ do
    (o, c) <- choice (map (\oc@(o, _) -> oc <$ char o) brackets)
    msym <- optional sym
    _ <- optional wsn
    --vs <- many (sepElem pe)
    vs <- pe `endBy` elemSep
    -- _ <- optional wsn
    _ <- maybe (string "") string msym
    _ <- char c
    return $ Bracket o c msym vs

sepElem pe = choice [
    pe
    , elemSep *> pe 
    ]

elemSep = choice [
    --void $ symbol "," -- This will be parser by 'expr'
     wsn
    ,pure ()
    ]

brackets :: [(Char, Char)]
brackets = [('{', '}'), ('[', ']')] -- ('<','>'),('«','»')]

data Bracket e = Bracket
    { open, close :: Char
    , op :: Maybe Text
    , values :: [e]
    }
    deriving (Show, Eq, Ord,Functor)

{-
>>> prettyShow <$> parseMaybe (bracket signedInt) "{%%11\n   22%%}"
Just "{%%11\n   22%%}"
-}
instance (Pretty e) => Pretty (Bracket e) where
    pPrint (Bracket open close mop vs) =
        let op = txt . fromMaybe "" $ mop
         in chr open <> op <> vcat (map pPrint vs) <> op <> chr close
