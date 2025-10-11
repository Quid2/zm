{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices -ddump-deriv -ddump-to-file -dumpdir=dump_output #-}

module ZM.Parser.Pattern where

import Data.Comp
import ZM.Parser.BindF
import ZM.Parser.ConstrF
import ZM.Parser.LiteralF

{-
>>> import Text.Megaparsec
>>> import Data.Comp.Show
>>> import ZM.Parser.HasParser
>>> :set -XOverloadedStrings

>>> parseMaybe parser "A 'x' (33) (BV 11.11) " :: Maybe (Term ExpF)
Just (ConstrF A [(LChar 'x'),(LInteger 33),(ConstrF BV [(LFloat 11.11)])])

>>> parseMaybe parser "'c'" :: Maybe (Term ExpF)
Just (LChar 'c')

>>> parseMaybe parser "('c')" :: Maybe (Term ExpF)
Just (LChar 'c')

>>> parseMaybe parser "((((('c')))))" :: Maybe (Term ExpF)
Just (LChar 'c')

>>> parseMaybe parser "A B" :: Maybe (Term ConstrF)
Just (ConstrF A [(ConstrF B [])])

>>> parseMaybe parser "Pair A B" :: Maybe (Term ConstrF)
Just (ConstrF Pair [(ConstrF A [(ConstrF B [])])])

>>> parseMaybe parser "Flag T (F G)" :: Maybe (Term ConstrF)
Just (ConstrF Flag [(ConstrF T [(ConstrF F [(ConstrF G [])])])])

>>> parseMaybe parser "Flag {val=True,val2=False}" :: Maybe (Term ConstrF)
Just (ConstrNamedF Flag [val=(ConstrF True []),val2=(ConstrF False [])])

>>> parseMaybe parser "11" :: Maybe (Term LiteralF)
Just (LInteger 11)

>>> parseMaybe parser "Pair 11 ('c')" :: Maybe (Term ExpF)
Just (ConstrF Pair [(LInteger 11),(LChar 'c')])

>>> parseMaybe parser "Pair F (T H)" :: Maybe (Term ExpF)
Just (ConstrF Pair [(ConstrF F [(ConstrF T [(ConstrF H [])])])])

>>> parseMaybe parser "Pair 11 (Pair False True)" :: Maybe (Term ExpF)
Just (ConstrF Pair [(LInteger 11),(ConstrF Pair [(ConstrF False [(ConstrF True [])])])])

>>> parseMaybe parser "Pair _ p2@(Pair _a b)" :: Maybe (Term ExpF)
Just (ConstrF Pair [(Wild ""),(At "p2" (ConstrF Pair [(Wild "a"),(Var "b")]))])


{
  Cons a b -> b
}

[
  exp
  exp
]

do
  exp
  exp

do {exp; exp}

(App if exp)

3 : Int

If a line is indented, it is put in parenthesis.
All lines at the same indentation level are put in parenthesis and connected by ,

if
  exp
then
  exp
else
  exp

if (exp) then (exp) else (exp)

(. (. (. (. if True) then) 1) else) 2)

exp ?
exp :
exp

(exp) ? (exp) : (exp)

zero
zero
\$$

-}
type ExpF = LiteralF :+: BindF :+: ConstrF

