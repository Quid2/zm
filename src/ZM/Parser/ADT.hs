{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables    ,CPP   #-}

{- Parse ZM ADT declarations and values -}
module ZM.Parser.ADT
  ( adts
  , adt
  , parType
  , absReference
  , maybeNamedAbsRef
  , namedOrAbsRef
  -- , absId
  )
where

import Data.Maybe ( fromMaybe )
import Text.Megaparsec
import ZM
    ( convert,
      prettyShow,
      Convertible,
      Fields,
      Type(..),
      Pretty,
      Identifier,
      AbsRef(..) )
import ZM.Parser.Lexer ( localId, symbol, shake )
import ZM.Parser.Types
    ( Label,
      AtAbsName,
      AtId,
      Range,
      TypeName,
      ADTParts(ADTParts),
      Parser,
      asTypeName )
import ZM.Parser.Util ( mkAt, cpars, pars )

-- $setup
-- >>> import ZM.Parser.Util(parseDoc)

{-|Parse a, possibly empty, group of ZM ADT declarations.

>>> parseMaybe adts ""
Just []

>>> putStr $ prettyShow $ parseMaybe adts "Void;"
Just [Void@(0:0-3) =]

>>> putStr $ prettyShow $ parseMaybe adts "Void;Bool=False | True"
Just [Void@(0:0-3) =,
      Bool@(0:5-8) = False@(0:10-14) | True@(0:18-21)]
-}
adts :: Parser [ADTParts]
adts = sepEndBy adt (symbol ";")

{-| Parse a ZM ADT declaration.

>>> putStr $ prettyShow $ parseMaybe adt "Void"
Just Void@(0:0-3) =

>>> putStr $ prettyShow $ parseMaybe adt "Bool.K306f1981b41c = False \n| True"
Just Bool.K306f1981b41c@(0:0-17) = False@(0:21-25) | True@(1:2-5)

>>> putStr $ prettyShow $ parseMaybe adt "是不是 ≡   是\n                    | 不是"
Just 是不是@(0:0-2) = 是@(0:8) | 不是@(1:22-23)

>>> putStr $ prettyShow $ parseMaybe adt "A = A (A (B C D))"
Just A@(0:0) = A@(0:4) (A@(0:7) (B@(0:10) C@(0:12) D@(0:14)))

>>> putStr $ prettyShow $ parseMaybe adt "Msg = Msg {from:Name.K306f1981b41c,subject:String}"
Just Msg@(0:0-2) = Msg@(0:6-8) {from@(0:11-14) :: Name.K306f1981b41c@(0:16-33),
                                subject@(0:35-41) :: String@(0:43-48)}

>>> putStr $ prettyShow $ parseMaybe adt "List a = Cons {head:a, tail:List a} | \nNil"
Just List@(0:0-3) a@(0:5) = Cons@(0:9-12) {head@(0:15-18) :: a@(0:20),
                                           tail@(0:23-26) :: List@(0:28-31) a@(0:33)} | Nil@(1:0-2)


Syntax Errors:

>>> putStr $ prettyShow $ parseDoc adt "Bool = | True"
Left "unexpected '|' expecting end of input or letter"@(0:7)

>>> putStr $ prettyShow $ parseDoc adt " = "
Left "unexpected '=' expecting letter"@(0:1)

>>> putStr $ prettyShow $ parseDoc adt "a b.d e.f = c.f.a"
Left "unexpected '.' expecting '=', '_', '\8801', alphanumeric character, end of input, or letter"@(0:3)
-}
adt :: Parser ADTParts
adt =
  (\name vars mcons -> ADTParts name vars (fromMaybe [] mcons))
    <$> at namedMaybeAbsRef -- absIdAt
    <*> many idAt
    <*> optional ((symbol "=" <|> symbol "≡") *> sepBy constr (symbol "|"))

{-| Parse a constructor declaration (with either named or unnamed fields).

>> prettyShow <$> parseMaybe constr "False"
Just (ADTParts {name = "Bool"@(0,0), vars = [], constrs = [("False"@(0,7),Left []),("True"@(0,15),Left [])]})

>>> prettyShow <$> parseMaybe constr "Cons  a  ( List a  )"
Just "Cons@(0:0-3) a@(0:6) (List@(0:11-14) a@(0:16))"

>>> prettyShow <$> parseMaybe constr "Pos {}"
Just "Pos@(0:0-2) {}"

>>> prettyShow <$> parseMaybe constr "Cons {head:a, tail:List a}"
Just "Cons@(0:0-3) {head@(0:6-9) :: a@(0:11),\n              tail@(0:14-17) :: List@(0:19-22) a@(0:24)}"

>>> prettyShow <$> parseMaybe constr "V A B (C D)"
Just "V@(0:0) A@(0:2) B@(0:4) (C@(0:7) D@(0:9))"
-}
constr :: Parser (AtId, Fields AtId AtAbsName)
constr = (,) <$> idAt <*> flds
      --flds = eitherP unnamedFlds namedFlds


 where
  flds      = (Right <$> namedFlds) <|> (Left <$> unnamedFlds)
  namedFlds = cpars (sepBy namedConstrFld (symbol ","))
  namedConstrFld =
    (,) <$> (idAt <* (symbol "::" <|> symbol ":")) <*> parType absIdAt
  unnamedFlds = many (simpleType absIdAt) -- many typeAt

-- atyp = pos (typ name)
-- typeAt = typ absIdAt
absIdAt :: Parser (Label Range (TypeName Identifier))
absIdAt = at namedOrAbsRef

idAt :: Parser AtId
idAt = at localIdentifier

localIdentifier :: Convertible String b => Parser b
localIdentifier = convert <$> localId

{- Add location information to the result of a parser.

We assume that:
* the parser does not parse initial space
* the length of parsed text is equal to the length of the pretty-shown result
* the parsed text is disposed on a single line
-}

at :: (TraversableStream s, MonadParsec e s m, Pretty a2) => m a2 -> m (Label Range a2)



at parser = do
  pos <- getSourcePos
  r   <- parser
  return $ mkAt pos (length (prettyShow r)) r

-- at parser = do
--   pos1 <- getPosition
--   r <- parser
--   (mr,ms) <- match parser
--   let l = length . dropWhile (== ' ') . reverse $ ms
--   --pos2 <- getPosition
--   -- when (sourceLine pos2 /= sourceLine pos1) $ fail "at: unexpected multiline parser"
--   --return $ At (mkRange pos1 (unPos (sourceColumn pos2) - unPos (sourceColumn pos1))) r
--   return $ At (mkRange pos1 l) rr
{-| Parse a type application, a type constructor with zero or more parameters

>>> parseMaybe (parType localId) "a"
Just (TypeCon "a")

>>> parseMaybe (parType localId) "((a))"
Just (TypeCon "a")

>>> parseMaybe (parType localId) "a b"
Just (TypeApp (TypeCon "a") (TypeCon "b"))

>>> parseMaybe (parType localId) "(a) ((b)) c"
Just (TypeApp (TypeApp (TypeCon "a") (TypeCon "b")) (TypeCon "c"))

>>> typeN <$> parseMaybe (parType localId) "(a b (c d (e f g))) z"
Just (TypeN "a" [TypeN "b" [],TypeN "c" [TypeN "d" [],TypeN "e" [TypeN "f" [],TypeN "g" []]],TypeN "z" []])

>>> parseMaybe (parType localId) "(a b) c"
Just (TypeApp (TypeApp (TypeCon "a") (TypeCon "b")) (TypeCon "c"))
-}
parType :: Parser a -> Parser (Type a)
parType cons = foldl1 TypeApp <$> types cons

{-| Parse a simple type, either a type constructor or a type in parentheses
>>> parseMaybe (simpleType localId) "a"
Just (TypeCon "a")

>>> parseMaybe (simpleType localId) "(a b c)"
Just (TypeApp (TypeApp (TypeCon "a") (TypeCon "b")) (TypeCon "c"))
-}
simpleType :: Parser ref -> Parser (Type ref)
simpleType cons = pars (parType cons) <|> (TypeCon <$> cons)

{-| Parse a non-empty sequence of types.
>>> parseMaybe (types localId) ""
Nothing

>>> parseMaybe (types localId) "(a) ((b)) (c d) e"
Just [TypeCon "a",TypeCon "b",TypeApp (TypeCon "c") (TypeCon "d"),TypeCon "e"]
-}
types :: Parser ref -> Parser [Type ref]
types cons = some (simpleType cons)

-- typN :: Parser a -> Parser (TypeN a)
-- typN cons = pars (typN cons) <|> (TypeN <$> cons <*> many (typN cons))
{-| Parse a simple type name, possibly qualified with an absolute code.

>>> prettyShow <$> parseMaybe namedOrAbsRef "nil"
Just "nil"

The . is required to distinguish a hash from a plain type name (remove this ambiguity?)

This is a hash:

>>> parseMaybe namedOrAbsRef ".K306f1981b41c"
Just (That (AbsRef (SHAKE128_48 48 111 25 129 180 28)))

And this is just a type name (NO actually we try to interpret it as an absolute reference first):

>>> parseMaybe namedOrAbsRef "K306f1981b41c"
Just (That (AbsRef (SHAKE128_48 48 111 25 129 180 28)))
-}
-- absId :: Parser (TypeName Identifier)
-- absId =
--   asTypeName Nothing . Just <$> dref <|>
--   asTypeName <$> (Just <$> localIdentifier) <*> optional dref
namedOrAbsRef :: Parser (TypeName Identifier)
namedOrAbsRef =
  asTypeName Nothing
    .   Just
    <$> (optional (symbol ".") *> absReference)
    <|> asTypeName
    <$> (Just <$> localIdentifier)
    <*> optional dref

namedMaybeAbsRef :: Parser (TypeName Identifier)
namedMaybeAbsRef = asTypeName <$> (Just <$> localIdentifier) <*> optional dref

-- An absolute reference, plus optionally the data type name
{- |
>>> prettyShow <$> parseMaybe maybeNamedAbsRef "Bool.K306f1981b41c"
Just "Bool.K306f1981b41c"

>>> prettyShow <$> parseMaybe maybeNamedAbsRef ".K306f1981b41c"
Just ".K306f1981b41c"

>>> prettyShow <$> parseMaybe maybeNamedAbsRef "K306f1981b41c"
Just ".K306f1981b41c"

>>> prettyShow <$> parseMaybe maybeNamedAbsRef "Bool"
Nothing
-}
maybeNamedAbsRef :: Parser (TypeName Identifier)
maybeNamedAbsRef =
  asTypeName Nothing
    .   Just
    <$> (optional (symbol ".") *> absReference)
    <|> asTypeName
    <$> (Just <$> localIdentifier)
    <*> (Just <$> dref)

dref :: Parser AbsRef
dref = symbol "." *> absReference

{- |
>>> prettyShow <$> parseMaybe absReference "K306f1981b41c"
Just "K306f1981b41c"

>>> prettyShow <$> parseMaybe absReference "KKK"
Nothing
-}
absReference :: Parser AbsRef
absReference = AbsRef <$> shake

