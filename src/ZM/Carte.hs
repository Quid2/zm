{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ZM.Carte where

import Control.Monad.Combinators.Expr
import Data.Comp
import Data.Comp.Derive
import Data.Comp.Equality ()
import Data.Comp.Show ()
import Data.Fix (Fix)
import Data.Functor.Identity
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type Exp = AddF :+: SubF :+: LiteralF

type ExpF = Fix Exp

data AddF r = Add r r
  deriving (Functor)

data SubF r = Sub r r
  deriving (Functor)

{-
>>> iConstrF "True" [] :: Term ConstrF
(ConstrF "True" [])
-}
data ConstrF r
  = ConstrF
      -- | Name of the constructor (e.g. "True")
      Text
      -- | Constructor parameters, possibly named
      --  e.g. ConstrF "True" []
      [r]
  | ConstrNamedF Text [(Text, r)]
  deriving (Functor)

data LiteralF r
  = LInteger Integer
  | LFloat Double -- Rational
  | LChar Char
  | LString Text
  deriving (Functor)

$( derive
     [ makeTraversable,
       makeFoldable,
       makeEqF,
       makeShowF,
       smartConstructors,
       smartAConstructors
     ]
     [''AddF, ''SubF]
 )

$( derive
     [ makeTraversable,
       makeFoldable,
       makeEqF,
       makeShowF,
       smartConstructors,
       smartAConstructors
     ]
     [''ConstrF]
 )

$( derive
     [ makeTraversable,
       makeFoldable,
       makeEqF,
       makeShowF,
       smartConstructors,
       smartAConstructors
     ]
     [''LiteralF]
 )

{-
>>> parseMaybe expr "1 + (2 - (2+2))"
Just (Add (LInteger 1) (Sub (LInteger 2) (Add (LInteger 2) (LInteger 2))))

>>> iLInteger 11 :: Term LiteralF
(LInteger 11)
-}
expr :: Parser (Cxt h Exp a)
expr = makeExprParser term table <?> "expression"

term :: Parser (Cxt h Exp a)
term = parens expr <|> (iLInteger <$> integer) <?> "term"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

spc :: ParsecT Void Text Identity ()
spc = L.space space1 empty empty

symbol = L.symbol spc

lexeme = L.lexeme spc

integer = lexeme L.decimal

table :: [[Operator (ParsecT Void Text Identity) (Cxt h Exp a)]]
table =
  [ --  [ prefix "-" negate,
    --   prefix "+" id
    -- ],
    -- [postfix "++" (+ 1)],
    -- [ binary "*" (*),
    --   binary "/" div
    -- ],
    [ binary "+" iAdd,
      binary "-" iSub
    ]
  ]

binary name f = InfixL (f <$ symbol name)

prefix name f = Prefix (f <$ symbol name)

postfix name f = Postfix (f <$ symbol name)

-- Term evaluation algebra
class Eval f v where
  evalAlg :: Alg f (Term v)

$(derive [liftSum] [''Eval])

{- Lift the evaluation algebra to a catamorphism

>>> eval (iAdd (iLInteger 11)  (iLInteger 22) :: Term Exp) :: Term LiteralF
(LInteger 33)

>>> eval (iLInteger 11 :: Term Exp) :: Term LiteralF
(LInteger 11)

>>> (iLInteger 11) :: Term LiteralF
(LInteger 11)
-}
eval :: (Functor f, Eval f v) => Term f -> Term v
eval = cata evalAlg

instance {-# OVERLAPPABLE #-} (f :<: v) => Eval f v where
  evalAlg :: (f :<: v) => Alg f (Term v)
  evalAlg = inject -- default instance

instance {-# OVERLAPPABLE #-} (LiteralF :<: v) => Eval AddF v where
  evalAlg (Add x y) = iLInteger $ projInteger x + projInteger y

-- instance {-# OVERLAPPABLE #-} Eval AddF LiteralF where
--   evalAlg (Add x y) = iLInteger $ projInteger x + projInteger y

instance {-# OVERLAPPABLE #-} Eval AddF LiteralF where
  evalAlg (Add tx ty) =
    case (project tx, project ty) of
      (Just (LInteger x), Just (LInteger y)) -> iLInteger $ x + y
      (Just (LFloat x), Just (LFloat y)) -> iLFloat $ x + y
      _ -> error "AddF: unsupported types"

instance {-# OVERLAPPABLE #-} (LiteralF :<: v) => Eval SubF v where
  evalAlg :: (LiteralF :<: v) => Alg SubF (Term v)
  evalAlg (Sub x y) = iLInteger $ projInteger x - projInteger y

--   evalAlg (Mult x y) = iConst $ projC x * projC y
--   evalAlg (Fst x)    = fst $ projP x
--   evalAlg (Snd x)    = snd $ projP x

projInteger :: (LiteralF :<: v) => Term v -> Integer
projInteger v = case project v of Just (LInteger n) -> n

projFloat :: (LiteralF :<: v) => Term v -> Double
projFloat v = case project v of Just (LFloat n) -> n
