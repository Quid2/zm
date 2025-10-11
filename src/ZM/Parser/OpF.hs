{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ZM.Parser.OpF where

import Data.Comp.Derive
import Data.List
import Data.Text (Text, pack, unpack)
import Text.Megaparsec
import ZM.Parser.HasParser
import ZM.Parser.Lexer
import ZM.Parser.LiteralF
import ZM.Parser.Op
import ZM.Parser.ParseF
import ZM.Parser.Types
import ZM.Parser.Util

{-
>>> import Text.Megaparsec
>>> import ZM.Parser.LiteralF
>>> import Data.Comp
>>> import Data.Comp.Show
>>> :set -XTypeOperators
>>> type ExpF = LiteralF :+: PrefixF

:+: InfixF

>>> parseMaybe parser "add" :: Maybe (Term ExpF)
Just (Prefix "add" [])

>>> parseMaybe parser "add 11 22" :: Maybe (Term ExpF)
Just (Prefix "add" ["(LInteger 11)","(LInteger 22)"])

>>> parseMaybe parser "add (sub 23 44) zero" :: Maybe (Term ExpF)
Just (Prefix "add" ["(Prefix \"sub\" [\"(LInteger 23)\",\"(LInteger 44)\"])","(Prefix \"zero\" [])"])

1 + 1 * 5

App inc a
inc a
a .inc
a ++
++. a

(+)

(+ 2)

(2 +)

(1 `add`)

(1 `add` 2)

App (App + a) b
add a b
a .add b
a + b
+. a b

-}
-- data OpF r
--   = Prefix Text [r]
--   | InfixApp r Text r
--   deriving (Functor)

data PrefixF r = Prefix Text [r] deriving (Functor)

instance (HasParser r) => HasParser (PrefixF r) where
  parser = Prefix <$> prefixOp <*> many parser

data InfixF r = Infix r Text r deriving (Functor)

instance (HasParser r) => HasParser (InfixF r) where
  parser = Infix <$> parser <*> infixOp <*> parser

-- txt = fmap pack

$( derive
     [ makeTraversable,
       makeFoldable,
       makeEqF,
       makeShowF,
       smartConstructors,
       smartAConstructors
     ]
     [''InfixF, ''PrefixF]
 )

-- instance ParseF InfixF t where
--   parse top = iInfix <$> top <*> infixOp <*> top <?> "infix operation"
