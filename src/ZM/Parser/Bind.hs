{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module ZM.Parser.Bind
  ( Binder,
    binder,
  )
where

import qualified Data.Text as T
import Prettyprinter hiding ((<>))
import qualified Prettyprinter as PP
import Text.Megaparsec
import ZM.Parser.ADT (name)
import ZM.Parser.Lexer
import ZM.Parser.Literal
import ZM.Parser.Types
import ZM.Parser.Val
import ZM.Parser.HasParser

{-
>>> import Text.Megaparsec
>>> import ZM.Parser.Literal

>>> parseMaybe bind "_" :: Int
Variable not in scope:
  bind :: Parsec e0_a1cHv[tau:1] s0_a1cHw[tau:1] a0_a1cHx[tau:1]

>>> parseMaybe bind "_a"
Variable not in scope:
  bind :: Parsec e0_a1cJW[tau:1] s0_a1cJX[tau:1] a_a1cJY[sk:1]

>>> parseMaybe (pAt unsigned) "ab@(1)" :: Maybe (Binder Word)
Just (Bind "ab" 1)

>>> parseMaybe (binder unsigned) "ab@(1)" :: Maybe (Binder Word)
Couldn't match type `Binder r0_a1cZa[tau:1]' with `Word'
Expected: Parsec Void Text (Binder Word)
  Actual: ParsecT
            Void Text Identity (Binder (Binder r0_a1cZa[tau:1]))
In the first argument of `parseMaybe', namely `(binder unsigned)'
In the expression:
    parseMaybe (binder unsigned) "ab@(1)" :: Maybe (Binder Word)
In an equation for `it_a1cX4':
    it_a1cX4
      = parseMaybe (binder unsigned) "ab@(1)" :: Maybe (Binder Word)
-}

-- bind :: Parser (Val lit Binder) -> Parser (Val lit Binder)
-- bind pr =
--   ( (\w -> if T.null w then PWild else PBind w PWild)
--       <$> wild
--   )
--     <|> ( uncurry PBind
--             <$> pAt pr
--         )

-- pattern PWild :: Val lit Binder
-- pattern PWild = Fix (BinderF Wild)

-- -- pattern PBind :: T.Text -> Val lit Binder
-- pattern PBind :: T.Text -> Fix (ValF lit Binder) -> Fix (ValF lit Binder)
-- pattern PBind name r = Fix (BinderF (Bind name r))

-- binder :: Parser (Binder r) -> Parser (Binder r)
binder pr =
  ( (\w -> if T.null w then Wild else At w Wild)
      <$> wild
  )
    <|> pAt pr

-- <|> pr

pAt :: Parser r -> Parser (Binder r)
pAt pr = do
  patVar <- localId <* symbol "@("
  r <- pr
  _ <- symbol ")"
  return (At patVar r)

data Binder r
  = Wild
  | Var T.Text
  | At T.Text r
  deriving (Eq, Ord, Show)

instance (Pretty r) => Pretty (Binder r) where
  pretty Wild = "_"
  pretty (At n r) = pretty n <> "@" <> pretty r
