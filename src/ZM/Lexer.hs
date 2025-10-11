{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZM.Lexer
  ( -- * Lexer
    Token (..),
    LocatedToken (..),
    lex,
    -- , validRecordPrefix
    -- , validConstructorPrefix

    -- * Errors related to parsing
    ParseError (..),
  )
where

-- \| Adapted from Grace.Lexer
--
--    The main reason for a separate lexing step using is because we would like
--    to use @Earley@ for LR parsing, but @Earley@ is not fast enough to handle
--    character-by-character parsing.  Instead, we delegate lexing to a
--    lower-level parsing library that supports efficient bulk parsing
--    (@megaparsec@ in this case).
--
--    Also, we need to implement indent-sensitive lexing.

import Control.Applicative (empty, (<|>))
import Control.Exception.Safe (Exception (..))
import qualified Control.Monad as Monad
import Control.Monad.Combinators (many, manyTill, sepBy1)
import qualified Control.Monad.Combinators as Combinators
import Data.Char (isSpace)
import qualified Data.Char as Char
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle (..), try, (<?>))
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import ZM.Location (Location (..), Offset (..))
import qualified ZM.Location as Location
import ZM.Parser.Lexer (sym)
import Prelude hiding (lex)

-- import qualified Text.URI as URI

-- | Short-hand type synonym used by lexing utilities
type Parser = Megaparsec.Parsec Void Text

indentLen :: Parser Int
indentLen = Text.length <$> Megaparsec.takeWhileP (Just "initial white space") isHSpace

-- | Is it a horizontal space character?
isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'

-- FIX the comment bit, return comments as Tokens (or implement them using a binary operator)
space :: Parser ()
-- space = Lexer.space Megaparsec.Char.space1 (Lexer.skipLineComment "#") empty
space = Lexer.space Megaparsec.Char.hspace1 empty empty

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

parseLines :: Parser [Line]
parseLines = many parseLine <* Megaparsec.eof

parseLine :: Parser Line
parseLine = do
  indent <- indentLen
  tokens <- manyTill parseLocatedToken lineEnd
  return Line {..}

lineEnd :: Parser Text
-- lineEnd = Megaparsec.Char.eol <|> Text.empty <$ Megaparsec.eof

lineEnd = Megaparsec.Char.eol

parseToken :: Parser Token
parseToken =
  Combinators.choice
    [ -- `file` has to come before the lexer for `.` so that a file
      -- prefix of `.` or `..` is not lexed as a field access
      --   file
      -- , uri
      text,
      number,
      prefix,
      infixOp,
      constructor
      -- OpenAngle <$ symbol "<",
      -- CloseAngle <$ symbol ">",
      -- OpenBrace <$ symbol "{",
      -- CloseBrace <$ symbol "}",
      -- OpenBracket <$ symbol "[",
      -- CloseBracket <$ symbol "]",
      -- OpenParenthesis <$ symbol "(",
      -- CloseParenthesis <$ symbol ")"
    ]

parseLocatedToken :: Parser LocatedToken
parseLocatedToken = do
  start <- fmap Offset Megaparsec.getOffset
  token <- parseToken
  return LocatedToken {..}

-- parseLocatedTokens :: Parser [LocatedToken]
-- parseLocatedTokens = do
--   space
--   manyTill parseLocatedToken Megaparsec.eof

-- | Lex a complete expression

{-
>>> l = lex ""

>>> l ""
Right [Line {indent = 0, tokens = []}]

>>> l "\n"
Right [Line {indent = 0, tokens = []},Line {indent = 0, tokens = []}]

>>> l "\r"
Right [Line {indent = 0, tokens = []}]

>>> l "1"
Right [Line {indent = 0, tokens = [LocatedToken {token = IntLiteral 1, start = 0}]}]

>>> l "\n\r"
Right [Line {indent = 0, tokens = []},Line {indent = 0, tokens = []}]

>>> l "\r\n"
Right [Line {indent = 0, tokens = []},Line {indent = 0, tokens = []}]

>>> l "\n\n"
Right [Line {indent = 0, tokens = []},Line {indent = 0, tokens = []},Line {indent = 0, tokens = []}]

>>> l "1\n"
Right [Line {indent = 0, tokens = [LocatedToken {token = IntLiteral 1, start = 0}]},Line {indent = 0, tokens = []}]

>>> l "   12 \n 11 \n"
Right [Line {indent = 3, tokens = [LocatedToken {token = IntLiteral 12, start = 3}]},Line {indent = 1, tokens = [LocatedToken {token = IntLiteral 11, start = 8}]},Line {indent = 0, tokens = []}]

>>>  l "45.6E7 1.2"
Right [Line {indent = 0, tokens = [LocatedToken {token = RealLiteral 4.56e8, start = 0},LocatedToken {token = RealLiteral 1.2, start = 7}]}]

>>> l "\"txt\""
Right [Line {indent = 0, tokens = [LocatedToken {token = TextLiteral "txt", start = 0}]}]

>>> l "do\n A\n B\n  B1\n  B2\n C"
Right [Line {indent = 0, tokens = [LocatedToken {token = Prefix "do", start = 0}]},Line {indent = 1, tokens = [LocatedToken {token = Constructor "A", start = 4}]},Line {indent = 1, tokens = [LocatedToken {token = Constructor "B", start = 7}]},Line {indent = 2, tokens = [LocatedToken {token = Constructor "B1", start = 11}]},Line {indent = 2, tokens = [LocatedToken {token = Constructor "B2", start = 16}]},Line {indent = 1, tokens = [LocatedToken {token = Constructor "C", start = 20}]}]

>>> l "A True"
Right [Line {indent = 0, tokens = [LocatedToken {token = Constructor "A", start = 0},LocatedToken {token = Constructor "True", start = 2}]}]

>>> l "'abc'"
Right [Line {indent = 0, tokens = [LocatedToken {token = Infix "'", start = 0},LocatedToken {token = Prefix "abc", start = 1},LocatedToken {token = Infix "'", start = 4}]}]

>>> l "abc"
Right [Line {indent = 0, tokens = [LocatedToken {token = Prefix "abc", start = 0}]}]

>>> l "a b c"
Right [Line {indent = 0, tokens = [LocatedToken {token = Prefix "a", start = 0},LocatedToken {token = Prefix "b", start = 2},LocatedToken {token = Prefix "c", start = 4}]}]

>>> l "b + c"
Right [Line {indent = 0, tokens = [LocatedToken {token = Prefix "b", start = 0},LocatedToken {token = Infix "+", start = 2},LocatedToken {token = Prefix "c", start = 4}]}]

>>> l " + "
Right [Line {indent = 1, tokens = [LocatedToken {token = Infix "+", start = 1}]}]

>>> l "1 *^* \"k\""
Right [Line {indent = 0, tokens = [LocatedToken {token = IntLiteral 1, start = 0},LocatedToken {token = Infix "*^*", start = 2},LocatedToken {token = TextLiteral "k", start = 6}]}]

>>> l ".. +"
Right [Line {indent = 0, tokens = [LocatedToken {token = Infix "..", start = 0},LocatedToken {token = Infix "+", start = 3}]}]

>>> l " {1}"
Right [Line {indent = 1, tokens = [LocatedToken {token = OpenBrace, start = 1},LocatedToken {token = IntLiteral 1, start = 2},LocatedToken {token = CloseBrace, start = 3}]}]

>>> l "[ ] ( )"
Right [Line {indent = 0, tokens = [LocatedToken {token = OpenBracket, start = 0},LocatedToken {token = CloseBracket, start = 2},LocatedToken {token = OpenParenthesis, start = 4},LocatedToken {token = CloseParenthesis, start = 6}]}]

>>> l "(())"
Right [Line {indent = 0, tokens = [LocatedToken {token = OpenParenthesis, start = 0},LocatedToken {token = OpenParenthesis, start = 1},LocatedToken {token = CloseParenthesis, start = 2},LocatedToken {token = CloseParenthesis, start = 3}]}]

>>> l "a\nb"
Right [Line {indent = 0, tokens = [LocatedToken {token = Prefix "a", start = 0}]},Line {indent = 0, tokens = [LocatedToken {token = Prefix "b", start = 2}]}]
-}
-- nest :: [Line] ->

lex ::
  -- | Name of the input (used for error messages)
  String ->
  -- | Source code
  Text ->
  Either ParseError [Line]
lex name code =
  case Megaparsec.parse parseLines name (Text.snoc code '\n') of
    Left ParseErrorBundle {..} -> do
      let bundleError :| _ = bundleErrors

      let offset = Offset (Error.errorOffset bundleError)

      Left (LexingFailed (Location {..}))
    Right tokens -> do
      return tokens

number :: Parser Token
number =
  try parseInteger <|> parseScientific
  where
    parseInteger =
      IntLiteral
        <$> lexeme Lexer.decimal
        <* Megaparsec.notFollowedBy (Megaparsec.Char.char '.')
    parseScientific = do
      scientific <- lexeme Lexer.scientific
      return (RealLiteral scientific)

-- file :: Parser Token
-- file = lexeme do
--   prefix <- ("../" <|> ("" <$ "./") <|> "/") <?> "path character"

--   let isPath c =
--         '\x21' == c
--           || ('\x24' <= c && c <= '\x27')
--           || ('\x2A' <= c && c <= '\x2B')
--           || ('\x2D' <= c && c <= '\x2E')
--           || ('\x30' <= c && c <= '\x3B')
--           || '\x3D' == c
--           || ('\x40' <= c && c <= '\x5A')
--           || ('\x5E' <= c && c <= '\x7A')
--           || ('\x7C' == c)
--           || '\x7E' == c

--   let pathComponent = Megaparsec.takeWhile1P (Just "path character") isPath

--   suffix <- pathComponent `sepBy1` "/"

--   return (File (concatMap Text.unpack (prefix : List.intersperse "/" suffix)))

-- uri :: Parser Token
-- uri = (lexeme . try) do
--   u <- URI.parser

--   let schemes =
--         map (fromJust . URI.mkScheme) ["https", "http", "env", "file"]

--   if any (`elem` schemes) (URI.uriScheme u)
--     then return (URI u)
--     else fail "Unsupported Grace URI"

-- text :: Parser Token
text = lexeme do
  "\""

  let isText c =
        ('\x20' <= c && c <= '\x21')
          || ('\x23' <= c && c <= '\x5b')
          || ('\x5d' <= c && c <= '\x10FFFF')

  let unescaped = Megaparsec.takeWhile1P (Just "text character") isText

  let unicodeEscape = do
        "\\u"

        codepoint <- Combinators.count 4 Megaparsec.Char.hexDigitChar

        case Read.hexadecimal (Text.pack codepoint) of
          Right (n, "") -> do
            return (Text.singleton (Char.chr n))
          _ -> do
            fail "Internal error - invalid unicode escape sequence"

  let escaped =
        Combinators.choice
          [ "\"" <$ "\\\"",
            "\\" <$ "\\\\",
            "/" <$ "\\/",
            "\b" <$ "\\b",
            "\f" <$ "\\f",
            "\n" <$ "\\n",
            "\r" <$ "\\r",
            "\t" <$ "\\t",
            unicodeEscape
          ]
          <?> "escape sequence"

  texts <- many (unescaped <|> escaped)

  "\""

  return (TextLiteral (Text.concat texts))

{-
>>> map Char.isSymbol ['λ','∃','∀']
[False,True,True]

>>> map Char.isAlphaNum ['λ','∃','∀','爱']
[True,False,False,True]

>>> map Char.isLower ['λ','∃','∀','爱']
[True,False,False,False]

>>> map Char.isUpper ['λ','∃','∀','爱']
[False,False,False,False]
-}
isPrefix0 :: Char -> Bool
isPrefix0 = Char.isLower

-- \|| c == '_'

isPrefix :: Char -> Bool
isPrefix c = Char.isAlphaNum c || c == '_'

-- \|| c == '-' || c == '/'

-- | Returns `True` if the given record prefix is valid when unquoted
-- validRecordPrefix :: Text -> Bool
-- validRecordPrefix text_ =
--   case Text.uncons text_ of
--     Nothing ->
--       False
--     Just (h, t) ->
--       isPrefix0 h
--         && Text.all isPrefix t
--         && not (HashSet.member text_ reserved)

-- | Returns `True` if the given constructor prefix is a valid when unquoted
-- validConstructorPrefix :: Text -> Bool
-- validConstructorPrefix text_ =
--   case Text.uncons text_ of
--     Nothing ->
--       False
--     Just (h, t) ->
--       Char.isUpper h
--         && Text.all isPrefix t
--         && not (HashSet.member text_ reserved)

-- | Reserved tokens, which can't be used for prefixs unless they are quoted
-- reserved :: HashSet Text
-- reserved =
--   HashSet.fromList
--     [ "Constructors",
--       "Bool",
--       "gt"
--     ]
prefix :: Parser Token
prefix = (lexeme . try) do
  c0 <- Megaparsec.satisfy isPrefix0 <?> "prefix character"

  cs <- Megaparsec.takeWhileP (Just "prefix character") isPrefix

  let result = Text.cons c0 cs

  -- Monad.guard (not (HashSet.member result reserved))

  return (Prefix result)

infixOp = fmap Infix (lexeme sym)

constructor :: Parser Token
constructor = lexeme do
  c0 <- Megaparsec.satisfy Char.isUpper <?> "constructor character"

  cs <- Megaparsec.takeWhileP (Just "constructor character") isPrefix

  return (Constructor (Text.cons c0 cs))

-- quotedConstructor :: Parser Token
-- quotedConstructor = lexeme do
--   "'"

--   let isText c =
--         ('\x20' <= c && c <= '\x26')
--           || ('\x28' <= c && c <= '\x5b')
--           || ('\x5d' <= c && c <= '\x10FFFF')

--   let unescaped = Megaparsec.takeWhile1P (Just "constructor character") isText

--   let unicodeEscape = do
--         "\\u"

--         codepoint <- Combinators.count 4 Megaparsec.Char.hexDigitChar

--         case Read.hexadecimal (Text.pack codepoint) of
--           Right (n, "") -> do
--             return (Text.singleton (Char.chr n))
--           _ -> do
--             fail "Internal error - invalid unicode escape sequence"

--   let escaped =
--         Combinators.choice
--           [ "'" <$ "\\\'",
--             "\\" <$ "\\\\",
--             "/" <$ "\\/",
--             "\b" <$ "\\b",
--             "\f" <$ "\\f",
--             "\n" <$ "\\n",
--             "\r" <$ "\\r",
--             "\t" <$ "\\t",
--             unicodeEscape
--           ]
--           <?> "escape sequence"

--   texts <- many (unescaped <|> escaped)

--   "'"

--   return (Constructor (Text.concat texts))

{-
-}
data Nested = One Token | ListOf Nested
  deriving (Show)

-- | Tokens produced by lexing
data Token
  = RealLiteral Scientific
  | IntLiteral Int
  | TextLiteral Text
  | Constructor Text
  | Prefix Text
  | Infix Text
  | Bracketed Bracket [Token]
  -- \| OpenAngle
  -- \| OpenBrace
  -- \| OpenBracket
  -- \| OpenParenthesis
  -- \| CloseAngle
  -- \| CloseBrace
  -- \| CloseBracket
  -- \| CloseParenthesis
  --   | File FilePath
  --   | Optional
  --   | Or
  --   | Plus
  --   | SemiColon
  --   | Text
  --   | TextEqual
  --   | Then
  --   | Times
  --   | True_
  --   | Type
  -- \| URI URI.URI
  deriving stock (Eq, Show)

data Bracket = Round | Square | Curly
  deriving (Eq, Show)

-- | A token with offset information attached, used for reporting line and
--    column numbers in error messages
data LocatedToken = LocatedToken {token :: Token, start :: Offset}
  deriving (Show)

data Line = Line {indent :: Int, tokens :: [LocatedToken]}
  deriving (Show)

-- | Errors related to lexing and parsing
data ParseError
  = LexingFailed Location
  | ParsingFailed Location
  deriving (Eq, Show)

instance Exception ParseError where
  displayException (LexingFailed location) =
    Text.unpack
      (Location.renderError "Invalid input - Lexing failed" location)
  displayException (ParsingFailed location) =
    Text.unpack
      (Location.renderError "Invalid input - Parsing failed" location)
