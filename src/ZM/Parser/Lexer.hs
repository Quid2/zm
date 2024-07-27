{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ZM.Parser.Lexer (
    ws,
    wsn,
    lineEnd,
    eof,
    lexeme,
    -- $lexemes
    wild,
    identifier,
    sym,
    constr,
    localId,
    symbol,
    shake,
    -- unsigned,
) where

-- import Data.Word

import Data.Char as C
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as T
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import ZM hiding ()
import ZM.Parser.Types (Annotate (Ann), Label (..), Parser)

-- lexeme :: (AnnotatePos a b) => Parser a -> Parser b
-- lexeme p = do
--     pos <- getOffset
--     annotatePos pos <$> lexeme_ p

{- | Add trailing space removal to a parser
lexeme :: Parser a -> Parser a
-}
lexeme = L.lexeme ws

ws :: Parser ()
ws = L.space hspace1 empty empty

wsn = L.space space1 empty empty

{- | Space consumer
 |Removes spaces and haskell style line and block comments "--.." "{\- ..-\}"
-}

-- sc = L.space space1 lineComment blockComment
--   where
--     lineComment = L.skipLineComment ("--" :: Text)

--     blockComment = L.skipBlockCommentNested ("{-" :: Text) "-}"

-- end parser = (<* eof)
-- doc = between sc (sc >> eof)

{- $lexemes
Lexemes remove any trailing space, including comments:

>>> parseMaybe float "3.3  -- a nice float"
Just 3.3

but do not remove initial space:

>>> parseMaybe float "  3.3"
Nothing
-}

{- Parse and normalise linux/windows/mac line endings
>>> parseMaybe lineEnd "\n"
Just '\n'

>>> parseMaybe lineEnd "\r\n"
Just '\n'

>>> parseMaybe lineEnd "\r"
Just '\n'
-}
lineEnd :: Parser Char
lineEnd =
    choice
        [ char '\r' >> optional (char '\n') >> return '\n'
        , char '\n'
        ]

-- TODO:add check on
-- withPredicate
--   :: (a -> Bool)       -- ^ The check to perform on parsed input
--   -> String            -- ^ Message to print when the check fails
--   -> Parser a          -- ^ Parser to run
--   -> Parser a          -- ^ Resulting parser that performs the check
-- withPredicate f msg p = do
--   o <- getOffset
--   r <- p
--   if f r
--     then return r
--     else do
--       setOffset o
--       fail msg

localId :: Parser Text
localId = lexeme name

identifier :: Parser Text
identifier = lexeme (name <|> sym)

{-

FIX THIS?

>>> parseMaybe name "_asd"
Nothing

>>> parseMaybe name "abc12"
Just "abc12"

>>> parseMaybe name "abc1*"
Nothing

>>> Nothing == parseMaybe name "True"
True

>>> Nothing == parseMaybe name "_"
True
-}
name :: Parser Text
name = T.cons <$> lowerChar <*> takeWhileP (Just "alpha numeric or _") (\c -> isAlphaNum c || c == '_')

{-
Allow _ ?

>>> Nothing == parseMaybe constr "_"
True

>>> parseMaybe constr "N"

>>> parseMaybe constr "Nil"
Just "Nil"

>>> parseMaybe constr "abc1*"
Nothing
-}
constr :: Parser Text
constr = lexeme $ T.cons <$> upperChar <*> takeWhileP (Just "alpha numeric") isAlphaNum

{-
>>> parseMaybe infixOp "+"
Just "+"

>>> parseMaybe sym "-−"
Just "-\8722"

>>> parseMaybe sym "->"
Just "->"

>>> parseMaybe sym "+−+=×÷<√∊≠><&!`~!@#$%^&*+=:;'?,.-_/!*/+"
Just "+\8722+=\215\247<\8730\8714\8800><&!`~!@#$%^&*+=:;'?,.-_/!*/+"
-}
sym :: Parser Text
sym = pack <$> some symChar

-- TODO: spell out allowed categories
symChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
symChar =
    satisfy
        ( \c ->
            C.isSymbol c
                || elem (C.generalCategory c) [C.MathSymbol, C.OtherPunctuation, C.CurrencySymbol]
                || elem c ['-', '_']
        )
        <?> "sym"
{-# INLINE symChar #-}

{-
>>> let p = parseMaybe wild

>>> p "_"
Just ""

>>> p "__"
Just "_"

>>> p "___"
Just "__"

>>> p "_a"
Just "a"

>>> p "_是不是"
Nothing
-}
wild :: Parser Text
wild = lexeme $ T.tail <$> wld
  where
    wld = T.cons <$> char '_' <*> (fromMaybe T.empty <$> optional (name <|> wld))

{- |
Parse a specific string

>>> parseMaybe (symbol "=") ""
Nothing

>>> parseMaybe (symbol "=") "*"
Nothing

>>> parseMaybe (symbol "=") "= -- an equal sign"
Just "="

>>> parseMaybe (symbol "if then") "if then"
Just "if then"

>>> parseMaybe (symbol "Gold金en") "Gold金en"
Just "Gold\37329en"
-}
symbol :: Text -> Parser Text
symbol = L.symbol ws

{- |
Parse absolute reference's compact string format

>>> parseMaybe shake "Ke45682c11f7b"
Just (SHAKE128_48 228 86 130 193 31 123)

>>> parseMaybe shake "KE45682C11F7B "
Just (SHAKE128_48 228 86 130 193 31 123)
-}
shake :: Parser (SHAKE128_48 a)
shake = lexeme k
  where
    k =
        ( \k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 ->
            unPrettyRef [k0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12]
        )
            <$> char 'K'
            <*> hexDigitChar
            <*> hexDigitChar
            <*> hexDigitChar
            <*> hexDigitChar
            <*> hexDigitChar
            <*> hexDigitChar
            <*> hexDigitChar
            <*> hexDigitChar
            <*> hexDigitChar
            <*> hexDigitChar
            <*> hexDigitChar
            <*> hexDigitChar
