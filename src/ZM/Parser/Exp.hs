{-# LANGUAGE InstanceSigs #-}
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
import ZM.Pretty

{-
>>> p s = prettyShow <$> parseMaybe expr s

>>> p " [one two] "
Nothing

>>> p "[one two] = [1 2]"
Just "((= [(one two)]) [(1 2)])"

>>> p "{ true -> false}"
Nothing

>>> p "{(true -> false) (false->true) }"
Just "{(((-> true) false) ((-> false) true))}"

>>> p "{true -> false \n false->true}"
Just "{((-> true) ((-> (false false)) true))}"


>>> parseMaybe expr "double 10"
Just (App (Op "double") (Lit (LInteger 10)))

>>> parseMaybe expr "+"
Nothing

>>> parseMaybe expr "3 * 1"
Just (App (App (Op "*") (Lit (LInteger 3))) (Lit (LInteger 1)))

>>> parseMaybe expr "f 10"
Just (App (Op "f") (Lit (LInteger 10)))

>>> parseMaybe expr "[13 ('a') \"abc\" ]"
Just (Arr (Bracket {open = '[', close = ']', op = Nothing, values = [App (App (Lit (LInteger 13)) (Lit (LChar 'a'))) (Lit (LString "abc"))]}))

Anselm's rules are as follows:
Monadic/prefix operators bind stronger than dyadic ones
All dyadic operators associate to the right, except = < > that do not associate

3 + 4 + 7 == 3 + (4 + 7)

f 3 + 2
(+ ($ f 3) 2)

how to implement haskell's $ ?

>>> prettyShow <$> parseMaybe expr "a -> b -> c"
Just "((-> a) ((-> b) c))"

1 + (2 * (4 + 5))
>>> prettyShow <$> parseMaybe expr "1 + 2 * 4 + 5"
Just "((+ 1) ((* 2) ((+ 4) 5)))"

>>> parseMaybe expr "1 + 2 + 4"
Just (App (App (Op "+") (Lit (LInteger 1))) (App (App (Op "+") (Lit (LInteger 2))) (Lit (LInteger 4))))

>>> parseMaybe expr "1 * 2"
Just (App (App (Op "*") (Lit (LInteger 1))) (Lit (LInteger 2)))

>>> parseMaybe expr "f 1 (r 2 3)"
Just (App (App (Op "f") (Lit (LInteger 1))) (App (App (Op "r") (Lit (LInteger 2))) (Lit (LInteger 3))))

>>> parseMaybe expr "f"
Just (Op "f")

>>> parseMaybe expr "1"
Just (Lit (LInteger 1))
-}
expr :: Parser (ExpR r)
expr = do
  l <- expr1
  pInfixR expr1 l <|> return l

pInfixR :: Parser (ExpR r) -> ExpR r -> Parser (ExpR r)
pInfixR pTerm x = do
  f <- Op <$> infixOp
  y <- pTerm >>= \r -> pInfixR pTerm r <|> return r
  return $ App (App f x) y

expr1 :: Parser (ExpR r)
expr1 = app simple

app :: Parser (ExpR r) -> Parser (ExpR r)
app e = foldl1 App <$> some e

simple :: Parser (ExpR r)
simple =
  choice
    [ parenthesis expr
    , Arr <$> bracket expr
    , Op <$> prefixOp
    , Lit <$> literal
    ]

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
  | Op Text
  | -- | Infix Text
    Arr (Bracket (ExpR r))
  | Lit Literal
  deriving (Show, Eq)

instance Pretty (ExpR r) where
  pPrint (App f a) = chr '(' <> hsep [pPrint f, pPrint a] <> chr ')'
  pPrint (Op name) = txt name
  pPrint (Arr brk) = pPrint brk
  pPrint (Lit l) = pPrint l
