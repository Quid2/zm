{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ZM.Parser.LiteralF where

import Data.Comp.Derive
import Data.Text (Text)
import Text.Megaparsec
import ZM.Parser.HasParser
import ZM.Parser.Literal (charLiteral, signedFloat, signedInt, textLiteral)
import ZM.Parser.Types

data LiteralF r
  = LInteger Integer
  | LFloat Double -- Rational
  | LChar Char
  | LString Text
  deriving (Functor, Show)

instance HasParser (LiteralF r) where parser = literal

literal :: Parser (LiteralF r)
literal =
  try (LInteger <$> signedInt)
    <|> (LFloat <$> signedFloat)
    <|> (LChar <$> charLiteral)
    <|> (LString <$> textLiteral)

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
