{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ZM.Parser.Lexer (
    sc,
    eof,
    -- $lexemes
    var,
    identifier,
    sym,
    localId,
    symbol,
    float,
    charLiteral,
    stringLiteral,
    textLiteral,
    shake,
    signed,
    unsigned,
) where

-- TODO: CHECK WHY IT FAILS WITH EVAL PLUGIN

-- import Data.Word

import Control.Monad
import Data.Char as C
import Data.Scientific
import Data.Text (Text, pack, unpack)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import ZM hiding ()
import ZM.Parser.Types (Parser)

{- $lexemes
Lexemes remove any trailing space, including comments:

>>> parseMaybe float "3.3  -- a nice float"
Just 3.3

but do not remove initial space:

>>> parseMaybe float "  3.3"
Nothing
-}

{- |
>>> parseMaybe charLiteral "''"
Nothing

>>> parseMaybe charLiteral " 'a'"
Nothing

>>> parseMaybe charLiteral "'a' -- a comment"
Just 'a'

>>> parseMaybe charLiteral "'a'"
Just 'a'

>>> parseMaybe charLiteral "'\n'"
Just '\n'

>>> parseMaybe charLiteral "'金'"
Just '\37329'

>>> parseMaybe charLiteral "'\37329'"
Just '\37329'

>>> parseMaybe charLiteral "?a"
Just 'a'

>>> parseMaybe charLiteral "?🔥"
Just '\128293'

>>> parseMaybe charLiteral "?\t"
Just '\t'

>>> parseMaybe charLiteral "?\37329"
Just '\37329'
-}
charLiteral :: Parser Char
charLiteral =
    lexeme $
        choice
            [ char '?' *> L.charLiteral -- Ruby style
            , between (char '\'') (char '\'') L.charLiteral -- Not using this frees '' for strings
            ]

{- |
>>> parseMaybe stringLiteral "\"\""
Just ""

>>> parseMaybe singleLineText "\"abc\n金\37329\" "
WAS Just "abc\n\37329\37329"
NOW Nothing

>>> parseMaybe stringLiteral "a\\nb"
Nothing
-}
textLiteral :: Parser Text
textLiteral = lexeme . fmap pack $ try singleLineText <|> multiLineText

stringLiteral :: Parser String
stringLiteral = singleLineText

{- Multi Line Text/String

Anything between an opening "\n and a final \n":
..."
line1
line2
"

'"' in text do not need to be escaped unless is at the beginning of a line:

Last line end is not included in the return string (is part of the closing delimiter.

>>> parseMaybe multiLineText "\"\n x\"x \n\""
Just " x\"x "

>>> parseMaybe multiLineText "\"\nabc\ndef\n\""
Just "abc\ndef"

>>> parseMaybe multiLineText "\"\nabc\n\""
Just "abc"

>>> parseMaybe multiLineText "'\nabc\r\nfgh\n'"
Just "abc\r\nfgh"

>>> parseMaybe multiLineText "\"\n\n\""
Just ""

Can use either " or ' as delimiter, but must use identical delimeters:

>>> Nothing == parseMaybe multiLineText "'\nabc\n\""
True

TODO: generalise to template/quoting
-}
multiLineText :: Parser String
multiLineText = do
    delim <- strDelim <* lineEnd
    manyTill L.charLiteral (try $ lineEnd >> char delim)

singleLineText :: Parser String
singleLineText = do
    delim <- strDelim
    manyTill L.charLiteral (char delim)

strDelim :: Parser Char
strDelim = char '"' <|> char '\''

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

{- |
Parse signed floats or integers (as floats)

>>> parseMaybe float "3"
Just 3.0

>>> parseMaybe (float :: Parser Float)  "+3"
Just 3.0

>>> parseMaybe (float :: Parser Double) "-3"
Just (-3.0)

>>> parseMaybe float "3.6E+11"
Just 3.6e11

>>> parseMaybe float "-3.6E-11"
Just (-3.6e-11)

>>> parseMaybe float "-1E99999999"
Just (-Infinity)

Note: the decimal dot is not required

>>> parseMaybe float "35"
Just 35.0
-}
float :: (RealFloat a) => Parser a
float = toRealFloat <$> L.signed (return ()) (lexeme L.scientific)

-- TODO: add binary
-- unsigned = char '0' ( char 'x' >> L.hexadecimal <|>  char' 'b' >> L.binary)

{- |
@setup
>>> import Data.Word
>>> import Numeric.Natural

>>> parseMaybe unsigned "11" :: Maybe Word8
Just 11

>>> parseMaybe unsigned "11." :: Maybe Word8
Nothing

>>> parseMaybe unsigned "33455" :: Maybe Word8
Just 175

>>> parseMaybe unsigned "33455" :: Maybe Word16
Just 33455

>>> parseMaybe unsigned "33455" :: Maybe Word32
Just 33455

>>> parseMaybe unsigned "33455" :: Maybe Word64
Just 33455

>>> parseMaybe unsigned "33455" :: Maybe Word
Just 33455

>>> parseMaybe unsigned "334559923200232302133331312313131231231231231231231" :: Maybe Natural
Just 334559923200232302133331312313131231231231231231231

>>> parseMaybe unsigned "+334559923200232302133331312313131231231231231231231" :: Maybe Natural
Nothing

>>> parseMaybe unsigned "-334559923200232302133331312313131231231231231231231" :: Maybe Natural
Nothing

>>> parseMaybe unsigned "0xFF" :: Maybe Word8
Just 255

>>> parseMaybe unsigned "0XFF"
Just 255

TO ADD:
parseMaybe unsigned "334559923200232302131231321312312310103334045535353" :: Maybe Z.Word
Just 334559923200232302131231321312312310103334045535353

parseMaybe unsigned "0" :: Maybe Word7
Just 0

parseMaybe unsigned "0X7F" :: Maybe Word7
Just 127

parseMaybe unsigned "0XFF" :: Maybe Word7
Nothing
-}
unsigned :: (Integral a) => Parser a
unsigned = (char '0' >> (char 'x' <|> char 'X') >> L.hexadecimal) <|> integral

{- |
@setup
>>> import Data.Int

>>> parseMaybe signed "55" :: Maybe Int8
Just 55

>>> parseMaybe signed "+55" :: Maybe Int8
Just 55

>>> parseMaybe signed "-55" :: Maybe Int8
Just (-55)

>>> parseMaybe signed "+ 55" :: Maybe Int8
Nothing

>>> parseMaybe signed "- 55" :: Maybe Int8
Nothing

>>> parseMaybe signed "3455" :: Maybe Int16
Just 3455

>>> parseMaybe signed "-4433455" :: Maybe Int32
Just (-4433455)

>>> parseMaybe signed "+231231231233455" :: Maybe Int64
Just 231231231233455

>>> parseMaybe signed "-12312312" :: Maybe Int
Just (-12312312)

>>> parseMaybe signed "0xFF" :: Maybe Int8
Nothing

>>> parseMaybe signed "334559923200232302133331312313131231231231231231231" :: Maybe Integer
Just 334559923200232302133331312313131231231231231231231

>>> parseMaybe signed "+334559923200232302133331312313131231231231231231231" :: Maybe Integer
Just 334559923200232302133331312313131231231231231231231

>>> parseMaybe signed "-334559923200232302133331312313131231231231231231231" :: Maybe Integer
Just (-334559923200232302133331312313131231231231231231231)
-}
signed :: (Integral a) => Parser a
signed = L.signed (return ()) (lexeme integral)

integral :: (Integral a) => Parser a
integral = do
    d <- L.decimal
    notFollowedBy (char '.')
    return d

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

{- |
Parse a ZM localId (a unicode letter followed by zero or more unicode alphanumeric characters or '_')

>>> parseMaybe localId "*"
Nothing

>>> parseMaybe localId "1"
Nothing

>>> parseMaybe localId "A"
Just "A"

>>> parseMaybe localId "Gold金en"
Just "Gold\37329en"

>>> parseMaybe localId "是不是"
Just "\26159\19981\26159"

>>> parseMaybe localId "Bool -- a bool"
Just "Bool"

>>> parseMaybe localId "ant_13_"
Just "ant_13_"
-}
localId :: Parser Text
localId = lexeme name

identifier :: Parser Text
identifier = lexeme (name <|> sym)

name :: Parser Text
name = pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

sym :: Parser Text
sym = pack <$> some symChar

-- TODO: spell out allowed categories
symChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
symChar = satisfy (\c -> C.isSymbol c || elem (C.generalCategory c) [C.OtherPunctuation, C.CurrencySymbol]) <?> "sym"
{-# INLINE symChar #-}

{- |
>>> parseMaybe var "是不是"
Nothing

>>> parseMaybe var "_"
Just Nothing

>>> parseMaybe var "_a"
Just (Just "a")

>>> parseMaybe var "_是不是"
Just (Just "\26159\19981\26159")
-}
var :: Parser (Maybe Text)
var = lexeme (char '_' *> optional name)

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
symbol = L.symbol sc

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

{- | Space consumer
 |Removes spaces and haskell style line and block comments "--.." "{\- ..-\}"
-}
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment ("--" :: Text)

    blockComment = L.skipBlockCommentNested ("{-" :: Text) "-}"

-- end parser = (<* eof)
-- doc = between sc (sc >> eof)

-- | Add trailing space removal to a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
