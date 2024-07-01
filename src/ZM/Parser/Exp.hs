{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module ZM.Parser.Exp where
import Data.Text (Text)
import Text.Megaparsec
import ZM.Parser.Bracket
import ZM.Parser.Lexer
import ZM.Parser.Literal (Literal (..), literal)
import ZM.Parser.Types
import ZM.Parser.Util
import ZM.Pretty
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe)


{-
>>> pr = parseMaybe (doc expr)
>>> p = fmap prettyShow . pr
>>> up = fmap unAnn . pr
>>> sup = fmap prettyShow . up

>>> sup "{T->T\nF->{x->x}}"
Just "{((-> T) T) ((-> F) {((-> x) x)})}"

>>> sup " [\n    1\n     -22\n] \n"
Just "[1 -22]"

>>> up "-3.6E+11"
Just (F (Lit (LFloat (-3.6e11))))

>>> p "{1 + 2}"
Just "{((+@3 1@1)@3 2@5)@3}@0"

>>> p " [one two] "
Just "[(one@2 two@6)@2]@1"

>>> p "[one two] = [1 2]"
Just "((=@10 [(one@1 two@5)@1]@0)@10 [(1@13 2@15)@13]@12)@10"

>>> sup "{ true -> false}"
Just "{((-> true) false)}"

>>> sup $ "{1\n2}"
Just "{1 2}"

>>> sup "(1 :: Nil) -> 1"
Just "((-> ((:: 1) Nil)) 1)"

>>> sup "Cons 1 Nil -> 1"
Just "((-> ((Cons 1) Nil)) 1)"

>>> p "{true -> false \n false->true}"
Just "{((->@6 true@1)@6 false@9)@6 ((->@22 false@17)@22 true@24)@22}@0"

>>> p "{true -> false,false->true}"
Just "{((->@6 true@1)@6 ((,@14 false@9)@14 ((->@20 false@15)@20 true@22)@20)@14)@6}@0"

>>> up "{true -> false \n false->true}" == up "{true -> false,false->true}"
False

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
>>> prettyShow . unAnn <$> parseMaybe expr "1 + 2 * 4 + 5"
Just "((+ 1) ((* 2) ((+ 4) 5)))"

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

>>> prettyShow . unAnn <$> parseMaybe expr "[1 [11 22] 33  ]"
Just "[((1 [(11 22)]) 33)]"

>>>  parseMaybe expr "[1\n2]"
Just (Ann 0 (Arr (Bracket {open = '[', close = ']', op = Nothing, values = [Ann 1 (Lit (LInteger 1)),Ann 3 (Lit (LInteger 2))]})))

>>> prettyShow . unAnn <$> parseMaybe expr "1"
Just "1"

>>> prettyShow . unAnn <$> parseMaybe expr "f 1"
Just "(f 1)"

>>> prettyShow . unAnn <$> parseMaybe expr "Cons 1 Nil"
Just "((Cons 1) Nil)"

>>> prettyShow . unAnn <$> parseMaybe expr "1 2"
Just "(1 2)"

>>> parseMaybe expr "Nil"
Just (Ann 0 (Con "Nil"))

>>> prettyShow . unAnn <$> parseMaybe expr "+ 1"
Nothing
-}

type Exp = Annotate Offset ExpR

located :: Parser (ExpR Exp) -> Parser Exp
located p = Ann <$> getOffset <*> p

-- located p = do
--   beg <- getOffset
--   v <- p 
--   end <- getOffset
--   return $ Ann (Range (fromIntegral beg) (fromIntegral end)) v

{-
>>> p = fmap error . parseModule

>>> p "numbers"
-}
parseModule fileName = do
    src <- T.readFile $ concat ["../qq/qq-src/",fileName,".qq"]
    T.putStrLn src
    putStrLn (maybe "Nothing" (prettyShow . unAnn) (parseMaybe mdl src))

mdl :: Parser Exp
mdl = doc expr

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
    , con
    , lit
    ]

pre :: Parser Exp
pre = located $ Op <$> prefixOp

arr :: Parser Exp
arr = located $ Arr <$> bracket expr

inf :: Parser Exp
inf = located $ Op <$> infixOp

con :: Parser Exp
con = located $ Con <$> constr

lit :: Parser Exp
lit = located $ Lit <$> literal

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
  | Con Text -- Constructor (e.g. "True")   
  | Op Text -- Infix Text
  | Arr (Bracket r)
  | Lit Literal
  deriving (Show, Eq, Functor)

instance (Pretty r) => Pretty (ExpR r) where
  pPrint (App f a) = chr '(' <> hsep [pPrint f, pPrint a] <> chr ')'
  pPrint (Con name) = txt name
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

instance (Pretty l, Pretty (f (Annotate l f))) => Pretty (Annotate l f) where
    pPrint (Ann l f) = pPrint f <> chr '@' <> pPrint l

-- instance (Pretty (f (Annotate () f))) => Pretty (Annotate () f) where
--     pPrint (Ann () f) = pPrint f

