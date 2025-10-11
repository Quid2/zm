{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Earley where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.Fix
import Data.Char
-- import Data.Comp (Cxt (Term), Term)
-- import Data.Comp.Ops ((:+:) (..))
import Data.Comp (Cxt (Term), Term)
import Data.Comp.Ops
import Data.Data (Proxy (Proxy))
import qualified Data.HashSet as HS
import Data.Kind (Type)
import Data.Maybe
import Data.String
import Data.Text (Text, pack)
import GHC.Conc (par)
import Text.Earley
import Text.Earley.Mixfix
import ZM.Parser.Lexer (isInfixChar, lexeme)
import ZM.Parser.Literal (charLiteral, signedFloat, signedInt, textLiteral)
import ZM.Parser.LiteralF
import ZM.Parser.OpF
import ZM.Parser.Types
import ZM.Parser.Util (parenthesis)

class HasParser a where
  parserE :: Prod r String String a

-- parserG :: Prod r String String a -> Grammar r (Prod r String String a)
-- parserG :: Grammar r (Prod r String String a)
-- parserG = rule parserE

instance (HasParser (f a), HasParser (g a)) => HasParser ((:+:) f g a) where
  parserE = (Inl <$> parserE) <|> (Inr <$> parserE)

-- parserEE top = (Inl <$> top) <|> (Inr <$> top)

instance (HasParser (f (Term f))) => HasParser (Term f) where
  -- TODO: FIX this
  parserE = Term <$> parserE

{-
>>> fullParses (parser gram) $ tokenize "(3) * sum 5 7 (1 + 2)"
([Inf (Lit "3") "*" (Pre "sum" [Lit "5",Lit "7",Inf (Lit "1") "+" (Lit "2")])],Report {position = 12, expected = ["infix operation","prefix operation","(","literal"], unconsumed = []})
-}

data Exp
  = Pre String [Exp]
  | Inf Exp String Exp
  | Lit String
  deriving (Show) -- Functor)

gram :: Grammar r (Prod r String String Exp)
gram = mdo
  top <-
    rule
      $ namedToken "(" *> top <* namedToken ")"
        <|> pre top
        <|> ( Inf <$> top <*> infixOp <*> top <?>
                "infix operation"
            )
        <|> Lit <$> ("1" <|> "2" <|> "3" <|> "4" <|> "5" <|> "6" <|> "7" <|> "8" <|> "9" <|> "0")
      <?> "literal"
  return top

pre :: (IsString e) => Prod r e String Exp -> Prod r e String Exp
pre top = Pre <$> prefixOp <*> many top <?> "prefix operation"

{-
>>> import Data.Comp.Show

>>> report (parser expGrammy) $ tokenize "( )"
Report {position = 1, expected = ["prefix operation","("], unconsumed = [")"]}

>>> fullParses (parser expGrammy) $ tokenize "sum"
([Pre "sum" []],Report {position = 1, expected = ["prefix operation","("], unconsumed = []})

>>> fullParses (parser expGrammy) $ tokenize "(3) * sum 5 (1 + 2)"
([],Report {position = 1, expected = ["prefix operation","("], unconsumed = ["3",")","*","sum","5","(","1","+","2",")"]})

>>> fullParses (parser expGrammyF) $ tokenize "sum"
([(Prefix "sum" [])],Report {position = 1, expected = ["infix operation","prefix operation"], unconsumed = []})

>>> fullParses (parser (grammy expP)) $ tokenize "3 * sum 5 7 - 7"
([(Infix (Infix (LInteger 3) "*" (Prefix "sum" ["(LInteger 5)","(LInteger 7)"])) "-" (LInteger 7)),(Infix (LInteger 3) "*" (Infix (Prefix "sum" ["(LInteger 5)","(LInteger 7)"]) "-" (LInteger 7))),(Infix (LInteger 3) "*" (Prefix "sum" ["(LInteger 5)","(Infix (LInteger 7) \"-\" (LInteger 7))"]))],Report {position = 7, expected = ["infix operation","prefix operation"], unconsumed = []})

>>> fullParses (parser (grammy expPDirect)) $ tokenize "sum 5 7"
([(Prefix "sum" ["(LInteger 5)","(LInteger 7)"])],Report {position = 3, expected = ["prefix operation"], unconsumed = []})

>>> fullParses (parser expGrammyF) $ tokenize "(3) * sum 5 7 (1 + 2)"
([],Report {position = 0, expected = ["infix operation","prefix operation"], unconsumed = ["(","3",")","*","sum","5","7","(","1","+","2",")"]})

>>> fullParses (parser expGrammy0) $ tokenize "3 * sum 5 7 - 7"
([(Infix (Infix (LInteger 3) "*" (Prefix "sum" ["(LInteger 5)","(LInteger 7)"])) "-" (LInteger 7)),(Infix (LInteger 3) "*" (Infix (Prefix "sum" ["(LInteger 5)","(LInteger 7)"]) "-" (LInteger 7))),(Infix (LInteger 3) "*" (Prefix "sum" ["(LInteger 5)","(Infix (LInteger 7) \"-\" (LInteger 7))"]))],Report {position = 7, expected = ["infix operation","prefix operation"], unconsumed = []})
-}
z :: Report String [String]
z = report (parser expGrammy) $ tokenize "(3) * sum 5 7 (1 + 2)"

e :: Report String [String]
e = report (parser expGrammyF) $ tokenize "(3) * sum 5 7 (1 + 2)"

expGrammy :: Grammar r (Prod r String String Exp)
expGrammy = grammy (\top -> parP top <|> pre top)

grammy :: (Prod r e t a -> Prod r e t a) -> Grammar r (Prod r e t a)
grammy p = mfix $ rule . p

parP :: (Eq b, Data.String.IsString b) => Prod r b b a -> Prod r b b a
parP top = namedToken "(" *> top <* namedToken ")"

expPDirect :: Prod r String String (Term ExpF) -> Prod r String String (Term ExpF)
expPDirect top = prefixFParser top <|> literalFParser

prefixFParser top = iPrefix <$> txt prefixOp <*> many top <?> "prefix operation"

literalFParser = iLInteger . read <$> ("1" <|> "2" <|> "3" <|> "4" <|> "5" <|> "6" <|> "7" <|> "8" <|> "9" <|> "0")

expGrammyF :: Grammar r (Prod r String String (Term ExpF))
-- expGrammyF = grammy (\top -> parP top <|> expP top)
expGrammyF = grammy expP

-- expP :: (LiteralF :<: t, PrefixF :<: t, InfixF :<: t) => Prod r String String (Term t) -> Prod r String String (Term t)
expP :: Prod r String String (Term ExpF) -> Prod r String String (Term ExpF)
expP top = parserEE (Proxy :: Proxy (LiteralF ())) top <|> parserEE (Proxy :: Proxy (PrefixF ())) top <|> parserEE (Proxy :: Proxy (InfixF ())) top

-- Try to use original 'a la carte' definition of :<:, works
-- class (Functor sub, Functor sup) => sub :<: sup where
--   inj :: sub a -> sup a

-- instance (Functor f) => f :<: f where
--   inj = id

-- instance (Functor f, Functor g) => f :<: (f :+: g) where
--   inj = Inl

-- instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
--   inj = Inr . inj

-- WONT WORK because of overlapping instances
-- expGrammy0 :: Grammar r (Prod r String String (Term ExpF))
-- expGrammy0 = grammy (parserEE (Proxy :: Proxy (ExpF ())))

-- iiPrefix t vs = injT $ Prefix t vs

-- injT :: (sub :<: b, Functor f) => f (sub (Cxt a b c)) -> f (Cxt a b c)
-- injT = fmap (Term . inj)

expGrammy0 :: Grammar r (Prod r String String (Term ExpF))
expGrammy0 = grammy (parserEE (Proxy :: Proxy (ExpF ())))

class HasParserF (f :: Type -> Type) t where
  parserEE :: (f :<: t) => Proxy (f ()) -> Prod r String String (Term t) -> Prod r String String (Term t)

instance HasParserF PrefixF t where
  parserEE :: (PrefixF :<: t) => Proxy (PrefixF ()) -> Prod r String String (Term t) -> Prod r String String (Term t)
  parserEE _ top = iPrefix <$> fmap pack prefixOp <*> many top <?> "prefix operation"

instance HasParserF InfixF t where
  parserEE _ top = iInfix <$> top <*> txt infixOp <*> top <?> "infix operation"

instance HasParserF LiteralF t where
  parserEE _ _ = iLInteger . read <$> ("1" <|> "2" <|> "3" <|> "4" <|> "5" <|> "6" <|> "7" <|> "8" <|> "9" <|> "0")

instance (HasParserF f t, HasParserF g t, f :<: t, g :<: t) => HasParserF ((:+:) f g) t where
  parserEE _ top =
    parserEE (Proxy :: Proxy (f ())) top
      <|> parserEE (Proxy :: Proxy (g ())) top

prefixOp :: Prod r e String String
prefixOp = satisfy isPrefix

infixOp = satisfy isInfix

isPrefix :: String -> Bool
isPrefix (h : t) = isAlpha h && all isAlphaNum t

isInfix :: String -> Bool
isInfix = all isInfixChar

type ExpF = LiteralF :+: PrefixF :+: InfixF

-- instance (HasParser r) => HasParser (PrefixF r) where
--   parserE = Prefix <$> txt prefixOp <*> many parserE <?> "prefix operation"

-- instance (HasParser r) => HasParser (InfixF r) where
--   parserE = Infix <$> parserE <*> txt infixOp <*> parserE <?> "infix operation"

-- instance HasParser (LiteralF r) where
--   parserE = LInteger . read <$> ("1" <|> "2" <|> "3" <|> "4" <|> "5" <|> "6" <|> "7" <|> "8" <|> "9" <|> "0")

-- data ParP r = Par r deriving (Functor)

-- instance HasParser (ParF r) where
--   parserE = LInteger . read <$> ("1" <|> "2" <|> "3" <|> "4" <|> "5" <|> "6" <|> "7" <|> "8" <|> "9" <|> "0")

txt = fmap pack

-- instance (HasParser r) => HasParser (InfixF r) where
--   parser = Infix <$> parser <*> infixOp <*> parser

{-
>>> fullParses (parser expr) $ tokenize "mul (ab  + c ) v"
Variable not in scope:
  expr
    :: Grammar
         r_a1zy8[sk:3]
         (Prod r_a1zy8[sk:3] e_a1zxX[sk:1] [Char] a_a1zxY[sk:1])

first (map pretty) $

>>> first (map pretty) $  fullParses (parser grammar) $ tokenize "a b push2 and b pre "
(["(_and_ (__push2 a b) (_pre b))"],Report {position = 6, expected = [",","->","|-","+","*","and","and","pre"], unconsumed = []})
-}
holey :: String -> Holey String
holey "" = []
holey ('_' : xs) = Nothing : holey xs
holey xs = Just i : holey rest
  where
    (i, rest) = span (/= '_') xs

data Expr = V (Holey String) | App Expr [Expr]
  deriving (Show)

identTable :: [[(Holey String, Associativity)]]
identTable =
  (map . map)
    (first holey)
    [ [("_->_", RightAssoc)],
      [("_,_", NonAssoc)],
      [("if_then_else_", RightAssoc)],
      [("_|-_:_", NonAssoc)],
      [("_+_", LeftAssoc)],
      [("_*_", LeftAssoc)],
      [("_and_", NonAssoc)],
      [("_pre", NonAssoc)],
      [("__push2", NonAssoc)]
    ]

grammar :: Grammar r (Prod r String String Expr)
grammar = mdo
  ident <-
    rule $
      (V . pure . Just) <$> satisfy (not . (`HS.member` mixfixParts)) <?>
        "identifier"
  atom <-
    rule $
      ident
        <|> namedToken "(" *> expr <* namedToken ")"
  normalApp <-
    rule $
      atom
        <|> App <$> atom <*> some atom
  expr <- mixfixExpression table normalApp (App . V)
  return expr
  where
    table = map (map $ first $ map $ fmap namedToken) identTable
    mixfixParts =
      HS.fromList
        [ s | xs <- identTable, (ys, _) <- xs, Just s <- ys
        ]
        `mappend` HS.fromList ["(", ")"]

pretty :: Expr -> String
pretty (V ps) = concatMap (fromMaybe "_") ps
pretty (App e es) = "(" ++ pretty e ++ " " ++ unwords (map pretty es) ++ ")"

tokenize :: String -> [String]
tokenize "" = []
tokenize (' ' : xs) = tokenize xs
tokenize ('\n' : xs) = tokenize xs
tokenize (x : xs)
  | x `HS.member` special = [x] : tokenize xs
  | otherwise = (x : as) : tokenize bs
  where
    (as, bs) = break (`HS.member` special) xs
    special = HS.fromList "(), \n"

data Free f a
  = Pure a
  | Impure (f (Free f a))

data Zero r

-- data Identity a = Identity a
vz :: Free Zero ()
vz = Pure ()

data One r = One

-- data Maybe a = Nothing | Just a
vos :: [Free One ()]
vos = [Pure (), Impure One]

data C e r = C e -- data Const e a = Const e

-- data Error e a = Error e | Ok a
voc :: [Free (C Int) ()]
voc =
  [ Pure (),
    Impure (C 1)
  ]

data Identity r = Identity r

vis :: [Free Identity ()]
vis =
  [ Pure (),
    Impure (Identity (Pure ())),
    Impure (Identity (Impure (Identity (Pure ()))))
    --  ..
  ]
