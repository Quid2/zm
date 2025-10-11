{-# LANGUAGE OverloadedStrings #-}

module ZM.Parser.Value
  ( value,
    Value,
    -- pattern,
    -- Pattern,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import ZM.Parser.Lexer (constr, localId, symbol, wild)
import ZM.Parser.Literal
import ZM.Parser.Types
import ZM.Parser.Util
import ZM.Parser.Val hiding (Value)

-- | Generic ZM value, a constructor followed by optional, optionally named, fields.
-- data Value = Value String ValueFields
--   deriving (Show)
-- |Constructor fields
-- type ValueFields = Either [Value] [(String, Value)]
type Value = Val Literal Void1

-- Parse a document as a ZM value
--  valueD :: Parser Value
--  valueD = doc value

-- | A Pattern matches a subset of values of a ZM Type
-- type Pattern = Val Literal Binder

--
-- Parse a plain ZM value.
--
-- >>> parseMaybe value ""
-- Nothing
--
-- >>> parseMaybe value "_"
-- Nothing
--
-- >>> parseMaybe value "_bv"
-- Nothing
--
-- Special syntax for numbers,chars,strings:
-- >>> parseMaybe value "33" == Just (VInteger 33)
-- True
--
-- >>> parseMaybe value "-33" == Just (VInteger (-33))
-- True
--
-- >>> parseMaybe value "-33.45" == Just (VFloat (-33.45))
-- True
--
-- >>> parseMaybe value "3.3E-12" == Just (VFloat (3.3e-12))
-- True
--
-- >>> parseMaybe value "-3.3E-12" == Just (VFloat (-3.3e-12))
-- True
--
-- >>> parseMaybe value "'a'" == Just (VChar 'a')
-- True
--
-- >>> parseMaybe value "((\"abc\"))" == Just (VString "abc")
-- True
--
-- >>> parseMaybe value "False" == Just (Constr "False" (Left []))
-- False
--
-- >>> parseMaybe value "(Cons False (Cons True Nil))" == Just (Constr "Cons" (Left [Constr "False" (Left []),Constr "Cons" (Left [Constr "True" (Left []),Constr "Nil" (Left [])])]))
-- False
--
-- >>> parseMaybe value "Flag {val=True}" == Just (Constr "Flag" (Right [("val",Constr "True" (Left []))]))
-- False
--
-- >>> parseMaybe value "T False \"abc\" (True) (N {name='g'})" == Just (Constr "T" (Left [Constr "False" (Left []),VString "abc",Constr "True" (Left []),Constr "N" (Right [("name",VChar 'g')])]))
-- False
--
-- >>> parseMaybe value "T {left=False,right=X 'g' V0}" == Just (Constr "T" (Right [("left",Constr "False" (Left [])),("right",Constr "X" (Left [VChar 'g',Constr "V0" (Left [])]))]))
-- False
--
-- No type annotations:
-- BAD >>> parseMaybe value "False::Bool"
-- Nothing
value :: Parser Value
value = valueV special

{-
Parse a pattern.

A wildcard:

>>> parseMaybe pattern "_ " == Just PWild

A named variable (variables must start with an _):

>>> parseMaybe pattern "_A"
Nothing

== Just (PBind "A")
False

No pattern matching on constructor, a variable corresponds to a full value:

>>> parseMaybe pattern "_  Nil"
Nothing

Variables can appear at any level:

>>> parse pattern "" "(T _ (T2  _b 'N'))"
Right (Fix (ConstrF "T" (Left [Fix (BinderF Wild),Fix (ConstrF "T2" (Left [Fix (BinderF (Bind "b")),Fix (LitF (LChar 'N'))]))])))

== Right (Constr "T" (Left [PWild,Constr "T2" (Left [PBind "b",VChar 'N'])]))
False

>>> parse pattern "" "A"

>>> parse pattern "" "(T ab)"
Right (Fix (ConstrF "T" (Left [Fix (ConstrF "ab" (Left []))])))

>>> parse pattern "" "Info 1 \"OK\""
Right (Fix (ConstrF "Info" (Left [Fix (LitF (LInteger 1)),Fix (LitF (LString "OK"))])))

>>> parse (localId) "" "N"
Left (ParseErrorBundle {bundleErrors = TrivialError 0 (Just (Tokens ('N' :| ""))) (fromList [Label ('l' :| "owercase letter")]) :| [], bundlePosState = PosState {pstateInput = "N", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
-}

-- PROB: ambiguity between variable names and constructors, we require a '_' before variable name
-- pattern :: Parser Pattern
-- pattern = valueV (special <|> binds)

constrV, valueV :: Parser (Val lit binder) -> Parser (Val lit binder)
valueV v = parenthesis (valueV v) <|> v <|> constrV v
constrV v = Constr <$> constr <*> fieldsV v

special :: Parser (Val Literal binder)
special =
  try (VInteger <$> signedInt)
    <|> (VFloat <$> signedFloat)
    <|> (VChar <$> charLiteral)
    <|> (VString <$> textLiteral)

nestedValue :: Parser (Val lit binder) -> Parser (Val lit binder)
nestedValue v =
  parenthesis (valueV v) <|> v <|> (\n -> Constr n (Left [])) <$> localId

fieldsV,
  unnamedFields ::
    Parser (Val lit binder) ->
    Parser (Either [Val lit binder] [(Text, Val lit binder)])
-- fields :: Parser ValueFields
fieldsV v = namedFields v <|> unnamedFields v

-- | Parse unnamed fields
unnamedFields v = Left <$> many (nestedValue v)

-- | Parse a set of named fields
--
-- \$setup
-- >>> let pflds = parseMaybe (namedFields value)
--
-- >>> pflds "{}"
-- Just (Right [])
--
-- Fields can be separated by commas:
--
-- >>> pflds "{a=False,b=True}" == Just (Right [("a",Constr "False" (Left [])),("b",Constr "True" (Left []))])
-- True
--
-- Or just by spaces, but you might need to use parenthesis to avoid ambiguity.
--
-- This might be interpreted as 'False b' and so it fails:
--
-- >>> pflds "{a=False b=True}"
-- Nothing
--
-- >>> pflds "{a=False\nb=True}"
--
-- >>> pflds "{a=(False) b=True}" == Just (Right [("a",Constr "False" (Left [])),("b",Constr "True" (Left []))])
-- True
--
-- >>> pflds "{a= Msg {from=Joe} b=True}"
-- Just (Right [("a",Fix (ConstrF "Msg" (Right [("from",Fix (ConstrF "Joe" (Left [])))]))),("b",Fix (ConstrF "True" (Left [])))])
--
-- Haskell style comments are allowed:
--
-- >>> pflds "{ l1  = cons false {\- we are in the middle of a Constr ()-\} nil , l2=  nil } -- named fields are completed" == Just (Right [("l1",Constr "cons" (Left [Constr "false" (Left []),Constr "nil" (Left [])])),("l2",Constr "nil" (Left []))])
-- True
namedFields ::
  Parser (Val lit binder) ->
  Parser (Either [Val lit binder] [(Text, Val lit binder)])
namedFields v = Right <$> cpars (sepBy (namedField v) (optional $ symbol ","))

namedField :: Parser (Val lit binder) -> Parser (Text, Val lit binder)
namedField v = do
  name <- localId
  _ <- symbol "="
  v <- valueV v
  return (name, v)
