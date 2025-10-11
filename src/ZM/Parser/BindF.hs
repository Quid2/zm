{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ZM.Parser.BindF where

import Data.Comp.Derive
import Data.Text (Text)
-- import qualified Data.Text as T
import Text.Megaparsec
import qualified Text.Megaparsec as Data.Comp
import ZM.Parser.HasParser
import ZM.Parser.Lexer
import ZM.Parser.LiteralF
import ZM.Parser.Types

data BindF r
  = Wild Text
  | Var Text
  | At Text r
  deriving (Functor)

{-
>>> import Text.Megaparsec
>>> import ZM.Parser.LiteralF
>>> import Data.Comp
>>> import Data.Comp.Show
>>> :set -XTypeOperators
>>> type ExpF = LiteralF :+: BindF

>>> parseMaybe parser "_" :: Maybe (Term ExpF)
Just (Wild "")

>>> parseMaybe parser "_abc" :: Maybe (Term ExpF)
Just (Wild "abc")

>>> parseMaybe parser "ab@(1)" :: Maybe (Term ExpF)
Just (At "ab" (LInteger 1))

>>> parseMaybe parser "ab" :: Maybe (Term ExpF)
Just (Var "ab")
-}

instance (HasParser r) => HasParser (BindF r) where parser = bind

bind :: (HasParser r) => Parser (BindF r)
bind = Wild <$> wild <|> pAt

pAt :: (HasParser r) => Parser (BindF r)
pAt = do
  var <- localId
  maybeR <- optional $ do
    _ <- symbol "@("
    r <- parser
    _ <- symbol ")"
    return r
  return $ case maybeR of
    Nothing -> Var var
    Just r -> At var r

$( derive
     [ makeTraversable,
       makeFoldable,
       makeEqF,
       makeShowF,
       smartConstructors,
       smartAConstructors
     ]
     [''BindF]
 )
