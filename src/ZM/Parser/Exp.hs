{-# LANGUAGE OverloadedStrings #-}

module ZM.Parser.Exp where

-- import Data.Fix
import Data.Text (Text)
import Text.Megaparsec
import ZM.Parser.Bracket
import ZM.Parser.Lexer
import ZM.Parser.Literal (Literal, literal)
import ZM.Parser.Types
import ZM.Parser.Util

{-
>>> parseMaybe expr "f 10"
Just (App (Prefix "f") (Lit (LInteger 10)))

>>> parseMaybe expr "[13 ('a') \"abc\" ]"
Just (Arr (Bracket {open = '[', op = Nothing, values = [App (App (Lit (LInteger 13)) (Lit (LChar 'a'))) (Lit (LString "abc"))]}))

Anselm's rules are as follows:
Monadic/prefix operators bind stronger than dyadic ones
All dyadic operators associate to the right, except = < >

3 + 4 + 7 == 3 + (4 + 7)


how to implement haskell's $ ?

-}

-- type Expr lit binder = Fix ExpR

data ExpR r
  = --     Cons
    --     -- | Name of the constructor (e.g. "True")
    --     String
    --     -- | Constructor parameters, possibly named
    --     --  e.g. ConstrF "True" []
    --     (Either [r] [(String, r)])

    -- | -- | A pattern that might bind a matched value to a name
    --   BinderF
    -- | -- \|A variable or a lit pattern (e.g. a string or a number)
    App (ExpR r) (ExpR r)
  | Prefix Text
  | Arr (Bracket (ExpR r))
  | Lit Literal
  deriving (Show, Eq)

expr :: Parser (ExpR r)
expr = app simple <|> simple

simple :: Parser (ExpR r)
simple =
  choice
    [ parenthesis expr
    , Arr <$> bracket expr
    , Lit <$> literal
    , Prefix <$> identifier
    ]

app :: Parser (ExpR r) -> Parser (ExpR r)
app e = foldl1 App <$> some e
