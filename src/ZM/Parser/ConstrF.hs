{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ZM.Parser.ConstrF where

import Data.Comp.Derive
import Data.List
import Data.Text (Text, unpack)
import Text.Megaparsec
import ZM.Parser.HasParser
import ZM.Parser.Lexer
import ZM.Parser.Types
import ZM.Parser.Util
import ZM.Parser.ParseF

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

instance (HasParser r) => HasParser (ConstrF r) where
  parser = constrV

-- instance ParseF ConstrF t where
--   parse top = iInfix <$> top <*> infixOp <*> top <?> "infix operation"


constrV :: (HasParser r) => Parser (ConstrF r)
constrV = do
  c <- constr
  ConstrNamedF c <$> namedFields <|> ConstrF c <$> unnamedFields

-- | Parse unnamed fields
unnamedFields :: (HasParser a) => Parser [a]
unnamedFields = many parser

namedFields :: (HasParser a) => Parser [(Text, a)]
namedFields = cpars (sepBy namedField (optional $ symbol ","))

{-
>>> parseMaybe namedField "x = 42" :: Maybe (Text, LiteralF ())
Just ("x",LInteger 42)

>>> parseMaybe namedField "x = 'c'" :: Maybe (Text, Char)
Just ("x",'c')
-}
namedField :: (HasParser a) => Parser (Text, a)
namedField = do
  name <- localId
  _ <- symbol "="
  v <- parser
  return (name, v)

-- Handcoded as generated instance is incorrect
instance ShowF ConstrF where
  showF (ConstrF name ps) =
    unwords ["(ConstrF", unpack name, "[" ++ intercalate "," ps ++ "])"]
  showF (ConstrNamedF name ps) =
    unwords ["(ConstrNamedF", unpack name, "[" ++ intercalate "," (map (\(n, v) -> unpack n ++ "=" ++ v) ps) ++ "])"]

$( derive
     [ makeTraversable,
       makeFoldable,
       makeEqF,
       --  makeShowF,
       smartConstructors,
       smartAConstructors
     ]
     [''ConstrF]
 )
