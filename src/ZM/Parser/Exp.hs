{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZM.Parser.Exp where

-- import Data.Fix
import Data.Text (Text)
import GHC.Desugar (AnnotationWrapper)
import Text.Megaparsec
import ZM.Parser.Bracket
import ZM.Parser.Lexer
import ZM.Parser.Literal (Literal (..), literal)
import ZM.Parser.Types
import ZM.Parser.Util
import ZM.Pretty

{-
>>> p s = prettyShow <$> parseMaybe expr s

>>> p " [one two] "
Nothing

>>> p "[one two] = [1 2]"
Just "((=@10 [(one@1 two@5)@1]@0)@10 [(1@13 2@15)@13]@12)@10"

>>> p "{ true -> false}"
Nothing

>>> p "{(true -> false) (false->true) }"
Just "{(((->@7 true@2)@7 false@10)@7 ((->@23 false@18)@23 true@25)@23)@7}@0"

>>> p "{true -> false \n false->true}"
Just "{((->@6 true@1)@6 ((->@22 (false@9 false@17)@9)@22 true@24)@22)@6}@0"

>>> parseMaybe expr "double 10"
Just (Ann 0 (App (Ann 0 (Op "double")) (Ann 7 (Lit (LInteger 10)))))

>>> parseMaybe expr "+"
Nothing

>>> parseMaybe expr "3 * 1"
Just (Ann 2 (App (Ann 2 (App (Ann 2 (Op "*")) (Ann 0 (Lit (LInteger 3))))) (Ann 4 (Lit (LInteger 1)))))

>>> parseMaybe expr "f 10"
Just (Ann 0 (App (Ann 0 (Op "f")) (Ann 2 (Lit (LInteger 10)))))

>>> parseMaybe expr "[13 ('a') \"abc\" ]"
Just (Ann 0 (Arr (Bracket {open = '[', close = ']', op = Nothing, values = [Ann 1 (App (Ann 1 (App (Ann 1 (Lit (LInteger 13))) (Ann 5 (Lit (LChar 'a'))))) (Ann 10 (Lit (LString "abc"))))]})))

Monadic/prefix operators bind stronger than dyadic ones

application associates to the left
f g 3 = f $ g $ 3 = (f $ g) $ 3

All dyadic operators associate to the right, except = < > that do not associate

3 + 4 + 7 == 3 + (4 + 7)

f 3 + 2
(+ ($ f 3) 2)

how to implement haskell's $ ?

>>> prettyShow <$> parseMaybe expr "a -> b -> c"
Just "((->@2 a@0)@2 ((->@7 b@5)@7 c@10)@7)@2"

1 + (2 * (4 + 5))
>>> prettyShow <$> parseMaybe expr "1 + 2 * 4 + 5"
Just "((+@2 1@0)@2 ((*@6 2@4)@6 ((+@10 4@8)@10 5@12)@10)@6)@2"

>>> parseMaybe expr "1 + 2 + 4"
Just (Ann 2 (App (Ann 2 (App (Ann 2 (Op "+")) (Ann 0 (Lit (LInteger 1))))) (Ann 6 (App (Ann 6 (App (Ann 6 (Op "+")) (Ann 4 (Lit (LInteger 2))))) (Ann 8 (Lit (LInteger 4)))))))

>>> parseMaybe expr "1 * 2"
Just (Ann 2 (App (Ann 2 (App (Ann 2 (Op "*")) (Ann 0 (Lit (LInteger 1))))) (Ann 4 (Lit (LInteger 2)))))

>>> parseMaybe expr "f 1 (r 2 3)"
Just (Ann 0 (App (Ann 0 (App (Ann 0 (Op "f")) (Ann 2 (Lit (LInteger 1))))) (Ann 5 (App (Ann 5 (App (Ann 5 (Op "r")) (Ann 7 (Lit (LInteger 2))))) (Ann 9 (Lit (LInteger 3)))))))

>>> parseMaybe expr "f"
Just (Ann 0 (Op "f"))

>>> parseMaybe expr "1"
Just (Ann 0 (Lit (LInteger 1)))
-}
type Exp = Annotate Int ExpR

located :: Parser (ExpR Exp) -> Parser Exp
located p = do
  pos <- getOffset
  Ann pos <$> p

expr :: Parser Exp
expr = do
  l <- expr1
  pInfixR expr1 l <|> return l

pInfixR :: Parser Exp -> Exp -> Parser Exp
pInfixR pTerm x = do
  f <- inf
  y <- pTerm >>= \r -> pInfixR pTerm r <|> return r
  return $ app (app f x) y

expr1 :: Parser Exp
expr1 = appN simple

appN :: Parser Exp -> Parser Exp
appN e = foldl1 app <$> some e

app :: Exp -> Exp -> Exp
app l@(Ann l1 _) r = Ann l1 (App l r)

simple :: Parser Exp
simple =
  choice
    [ parenthesis expr
    , arr
    , pre
    , lit
    ]

pre :: Parser Exp
pre = located $ Op <$> prefixOp

arr :: Parser Exp
arr = located $ Arr <$> bracket expr

inf = located $ Op <$> infixOp

lit = located $ Lit <$> literal

f1, v1, a1 :: Exp
-- e1 :: ExpR r
f1 = Ann 10 (Op "plus1")
v1 = Ann 10 (Lit $ LInteger 3)
a1 = Ann 10 (App f1 a1)

-- type Expr = Fix ExpR
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
    App r r
  | Op Text -- Infix Text
  | Arr (Bracket r)
  | Lit Literal
  deriving (Show, Eq)

instance (Pretty r) => Pretty (ExpR r) where
  pPrint (App f a) = chr '(' <> hsep [pPrint f, pPrint a] <> chr ')'
  pPrint (Op name) = txt name
  pPrint (Arr brk) = pPrint brk
  pPrint (Lit l) = pPrint l

-- located :: Parser LocatedToken
-- located p = do
--     start <- fmap Offset Megaparsec.getOffset
--     token <- parseToken
--     return LocatedToken{..}

-- parseLocatedTokens :: Parser [LocatedToken]
-- parseLocatedTokens = do
--     ws
--     manyTill parseLocatedToken Megaparsec.eof

-- class AnnotatePos val out where
--     annotatePos :: Int -> val -> out

-- instance AnnotatePos val val where annotatePos _ v = v

-- instance AnnotatePos val (Label Int val) where annotatePos = Label

-- located :: (a -> ExpR r) -> Parser (ExpR r) -> Parser (Annotate Int ExpR)

-- located :: Parser (ExpR r) -> Parser Exp
