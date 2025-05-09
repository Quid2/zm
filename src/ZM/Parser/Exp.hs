{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

module ZM.Parser.Exp where

import           Data.Bifunctor
import           Data.Text         (Text)
import qualified Data.Text         as T

-- import qualified Data.Text.IO as T
import           Prettyprinter
import           Text.Megaparsec
import           ZM.Parser.Bracket (Bracket, bracket, prettyBracket)
import           ZM.Parser.Lexer
import           ZM.Parser.Literal (Literal (..), literal)
import           ZM.Parser.Op
import           ZM.Parser.Types
import           ZM.Parser.Util

-- import ZM.Parser (ADTParts(name))

{- $setup
>>> pr = parseMaybe (doc expr)
>>> p = fmap pretty . pr
>>> up = fmap unAnn . pr
>>> sup = fmap pretty . up
-}

{-
>>> sup "{T->T\nF->{x->x}}"
Just {
  -> T T
  -> F {
           -> x x
  }
}

>>> sup " [\n    1\n     -22\n] \n"
Just [
  1
  -22
]

>>> up "-3.6E+11"
Just (F (Lit (LFloat (-3.6e11))))

>>> p "{1 + 2}"
Just {
  + 1@1 2@5@1
}@0

>>> p " [one two] "
Just [
  one@2 two@6@2
]@1

>>> p "[one two] = [1 2]"
Just = [
    one@1 two@5@1
]@0 [
      1@13 2@15@13
]@12@0

>>> sup "{ true -> false}"
Just {
  -> true false
}

>>> sup $ "{1\n2}"
Just {
  1
  2
}

>>> sup "(1 :: Nil) -> 1"
Just -> (:: 1 Nil) 1

>>> sup "Cons 1 Nil -> 1"
Just -> Cons 1 Nil 1

>>> p "{true -> false \n false->true}"
Just {
  -> true@1 false@9@1
  -> false@17 true@24@17
}@0

>>> p "{true -> false,false->true}"
Just {
  -> true@1 , false@9 -> false@15 true@22@15@9@1
}@0

>>> up "{true -> false \n false->true}" == up "{true -> false,false->true}"
False

>>> p "double 10"
Just double@0 10@7@0

>>> p "+"
Nothing

>>> p "3 * 1"
Just * 3@0 1@4@0

>>> p "f 10"
Just f@0 10@2@0

>>> p "[13 ('a') \"abc\" ]"
Just [
  13@1 (?a@5)@4@1 "abc"@10@1
]@0

Monadic/prefix operators bind stronger than dyadic ones

application associates to the left
f g 3 = f $ g $ 3 = (f $ g) $ 3

All dyadic operators associate to the right, except = < > that do not associate

3 + 4 + 7 == 3 + (4 + 7)

f 3 + 2
(+ ($ f 3) 2)

how to implement haskell's $ ?

>>> up "a -> b -> c"
Just (F (InfixApp (F (Prefix "a")) "->" (F (InfixApp (F (Prefix "b")) "->" (F (Prefix "c"))))))

1 + (2 * (4 + 5))
>>> up "1 + 2 * 4 + 5"
Just (F (InfixApp (F (Lit (LInteger 1))) "+" (F (InfixApp (F (Lit (LInteger 2))) "*" (F (InfixApp (F (Lit (LInteger 4))) "+" (F (Lit (LInteger 5)))))))))

>>> p "1 + 2 + 4"
Just + 1@0 + 2@4 4@8@4@0

>>> p "1 * 2"
Just * 1@0 2@4@0

>>> p "f 1 (r 2 3)"
Just f@0 1@2@0 (r@5 2@7@5 3@9@5)@4@0

>>> p "f"
Just f@0

>>> p "1"
Just 1@0

>>> sup "[1 [11 22] 33  ]"
Just [
  1 [
        11 22
  ] 33
]

>>>  p "[1\n2]"
Just [
  1@1
  2@3
]@0

>>> up "1"
Just (F (Lit (LInteger 1)))

>>> up "f 1"
Just (F (App (F (Prefix "f")) (F (Lit (LInteger 1)))))

>>> up "Cons 1 Nil"
Just (F (App (F (App (F (Con "Cons")) (F (Lit (LInteger 1))))) (F (Con "Nil"))))

>>> up "1 2"
Just (F (App (F (Lit (LInteger 1))) (F (Lit (LInteger 2)))))

>>> p "Nil"
Just Nil@0

>>> p "[+ 1 +]"
Just [+
   1@3
+]@0

>>> sup "{| x * 11 |}"
Just {|
   * x 11
\|}

>>> up "+ 1"
Nothing

>>> up "1 + 2 + 3"
Just (F (InfixApp (F (Lit (LInteger 1))) "+" (F (InfixApp (F (Lit (LInteger 2))) "+" (F (Lit (LInteger 3)))))))
-}

{-
>>> tt "rec"

>>> tt "parser"
-}
-- tt mdlName = loadMdl $ concat ["../qq/qq-src/", mdlName, ".qq"]

-- loadMdl :: FilePath -> IO ()
-- loadMdl fileName = do
--   src <- T.readFile fileName
--   case testPretty src of
--     Left "no parse" -> parseTest mdl src
--     Left m -> putStr m
--     Right src2 -> T.writeFile fileName src2

parseMdl :: T.Text -> Either String Exp
parseMdl = first errorBundlePretty . runParser mdl ""

parseMdlF :: Text -> Either String (F ExpR)
parseMdlF = fmap annToF . parseMdl

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
-- parseModule fileName = do
--   src <- T.readFile $ concat ["../qq/qq-src/", fileName, ".qq"]
--   T.putStrLn src
--   putStrLn (maybe "Nothing" (pretty . unAnn) (parseMaybe mdl src))

mdl :: Parser Exp
mdl = doc expr

{-
>>> sup "{|x=1 \n{34 \n{%% 11 %%}\n 9}\n y=2|}"
Just {|
  x = 1
  {
    34
    {%%
      11
    %%}
    9
  }
  y = 2
\|}

-}
expr :: Parser Exp
expr = do
  l <- expr1
  pInfixR expr1 l <|> return l

pInfixR :: Parser Exp -> Exp -> Parser Exp
pInfixR pTerm x@(Ann xAnn _) = try $ do
  -- f <- inf
  f <- infixOp
  y <- pTerm >>= \r -> pInfixR pTerm r <|> return r
  -- return $ app (app f x) y
  return $ Ann xAnn $ InfixApp x f y

expr1 :: Parser Exp
expr1 = appN simple

appN :: Parser Exp -> Parser Exp
appN e = foldl1 app <$> some e

app :: Exp -> Exp -> Exp
app l@(Ann l1 _) r = Ann l1 (App l r)

simple :: Parser Exp
simple =
  choice
    [ par
    , arr
    , pre
    , con
    , lit
    , wld
    ]

par :: Parser Exp
-- par = located $ Par <$> parenthesis expr
par = parenthesis expr

wld :: Parser Exp
wld = located $ Wild <$> wild

pre :: Parser Exp
pre = located $ Prefix <$> prefixOp

arr :: Parser Exp
arr = located $ Arr <$> bracket expr

-- inf :: Parser Exp
-- inf = located $ Infix <$> infixOp

con :: Parser Exp
con = located $ Con <$> constr

lit :: Parser Exp
lit = located $ Lit <$> literal

-- TODO: single InfixApp
-- type Expr = Fix ExpR
data ExpR r
  = App r r
  | -- Universal App: App+Infix App + Section (What about functions with a single arg?)

    -- | App Text (These r r)
    -- ?Or App r (These r r)
    Prefix Text
  | InfixApp r Text r
  | Wild Text -- Wildcard (without initial _)
  | Con Text -- Constructor (e.g. "True")
  | -- \| Par r

    -- | Infix Text
    -- Field Text
    Arr (Bracket r)
  | Lit Literal
  deriving (Show, Eq, Functor)

instance (Pretty a, PrettyArg (f (Annotate a f))) => PrettyArg (Annotate a f) where
  prettyArg t (Ann a f) = prettyArg t f <> pretty '@' <> pretty a

instance (PrettyArg (f (F f))) => PrettyArg (F f) where
  prettyArg t (F f) = prettyArg t f

instance (PrettyArg r) => Pretty (ExpR r) where
  pretty = prettyArg NoArg

class PrettyArg a where prettyArg :: Arg -> a -> Doc ann

data Arg = NoArg | PreArg | InfArg deriving (Show, Eq)

{-
>>> sup "foo bar  + big bop"
Just foo bar + big bop

>>> pr "x = y"
Just (Ann 0 (InfixApp (Ann 0 (Prefix "x")) "=" (Ann 4 (Prefix "y"))))

>>> sup "[[]]"
Just [
   [
   ]
]

>>> sup "Z -> y -> y"
Just Z -> (y -> y)

>>> tt "rec"

>>> tt "add"
-}
instance (PrettyArg r) => PrettyArg (ExpR r) where
  prettyArg arg =
    let
      no = prettyArg NoArg
      pre = prettyArg PreArg
      inf = prettyArg InfArg
      onInf InfArg d = par d
      onInf _ d      = d
      onPre PreArg d = par d
      onPre _ d      = d
      par d = "(" <> d <> ")"
     in
      \case
        InfixApp l op r -> onInf arg $ hsep [inf l, pretty op, inf r]
        App f a -> onPre arg $ hsep [no f, pre a]
        Con name -> pretty name
        Arr brk -> prettyBracket no brk
        Prefix name -> pretty name
        Wild name -> "_" <> pretty name
        -- Field name -> pretty name
        Lit l -> pretty l

{-
pat =>  lpat qconop pat (infix constructor)
        | lpat

lpat => apat | - (integer | float) (negative literal) | gcon apat1 ... apatk (arity gcon = k, k >= 1)

apat => var [ @ apat] (as pattern)
      | gcon (arity gcon = 0)
      | qcon { fpat1 ... fpatk } (labeled pattern, k >= 0)
      | literal
      | _ (wildcard)
      | ( pat ) (parenthesized pattern)
      | ( pat1 ... patk ) (tuple pattern, k >= 2)
      | [ pat1 ... patk ] (list pattern, k >= 1)
      | ˜ apat (irrefutable pattern)

fpat => qvar = pat
-}
