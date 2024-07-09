{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module ZM.Parser.Literal (
    Literal (..),
    literal,
    signedInt,
    signedFloat,
    unsigned,
    charLiteral,
    textLiteral,
    singleLineText,
    multiLineText,
) where

import Data.Scientific
import Data.Text
import qualified Data.Text as T
import Prettyprinter
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import ZM.Parser.Lexer
import ZM.Parser.Types

data Literal -- e
    = LInteger Integer
    | LFloat Double -- Rational
    | LChar Char
    | LString Text
    deriving (Eq, Ord, Show)

-- instance Pretty Literal where
--     pPrint (LInteger i) = pPrint i
--     pPrint (LFloat f) = pPrint f
--     pPrint (LChar c) = chr c
--     pPrint (LString t) = doubleQuotes $ txt t

{- |
>>> p = parseMaybe literal

>>> p "-9e+11"
Just (LFloat (-9.0e11))

>>> parseMaybe literal "13"
Just (LInteger 13)

>>> parseMaybe literal "-13"
Just (LInteger (-13))

>>> parseMaybe literal "13.55"
Just (LFloat 13.55)

>>> parseMaybe literal "'a'"
Just (LChar 'a')

>>> parseMaybe literal "\"abc\""
Just (LString "abc")
-}
literal :: Parser Literal
literal =
    choice
        [ LInteger <$> try signedInt
        , LFloat <$> try signedFloat
        , LChar <$> charLiteral
        , LString <$> textLiteral
        ]

instance Pretty Literal where
    pretty (LInteger n) = pretty n
    pretty (LFloat n) = pretty n
    pretty (LChar c) = pretty ['?', c]
    pretty (LString t) = prettyTextLiteral t

-- scalar = id

{- | Pretty-print a @Text@ literal

Copied from Grace.Type
-}
prettyTextLiteral :: Text -> Doc ann
prettyTextLiteral text =
    "\""
        <> ( pretty
                . T.replace "\"" "\\\""
                . T.replace "\b" "\\b"
                . T.replace "\f" "\\f"
                . T.replace "\n" "\\n"
                . T.replace "\r" "\\r"
                . T.replace "\t" "\\t"
                . T.replace "\\" "\\\\"
           )
            text
        <> "\""

{- |
@setup
>>> import Data.Int

>>> parseMaybe signedInt "55 " :: Maybe Int8
Just 55

>>> parseMaybe signedInt " 55" :: Maybe Int8
Nothing

>>> parseMaybe signedInt "+55" :: Maybe Int8
Just 55

>>> parseMaybe signedInt "-55" :: Maybe Int8
Just (-55)

>>> parseMaybe signedInt "+ 55" :: Maybe Int8
Nothing

>>> parseMaybe signedInt "- 55" :: Maybe Int8
Nothing

>>> parseMaybe signedInt "3455" :: Maybe Int16
Just 3455

>>> parseMaybe signedInt "-4433455" :: Maybe Int32
Just (-4433455)

>>> parseMaybe signedInt "+231231231233455" :: Maybe Int64
Just 231231231233455

>>> parseMaybe signedInt "-12312312" :: Maybe Int
Just (-12312312)

>>> parseMaybe signedInt "0xFF" :: Maybe Int8
Nothing

>>> parseMaybe (try signedInt <|> const 0 <$> symbol "+") "+"
Just 0

>>> parseMaybe signedInt "334559923200232302133331312313131231231231231231231" :: Maybe Integer
Just 334559923200232302133331312313131231231231231231231

>>> parseMaybe signedInt "+334559923200232302133331312313131231231231231231231" :: Maybe Integer
Just 334559923200232302133331312313131231231231231231231

>>> parseMaybe signedInt "34 "
Just 34

>>> parseMaybe signedInt "-3345599232002323021333313123131312312312312312312315" :: Maybe Integer
Just (-3345599232002323021333313123131312312312312312312315)
-}
signedInt :: (Integral a) => Parser a
signedInt = lexeme $ L.signed (return ()) integral

integral :: (Integral a) => Parser a
integral = do
    d <- L.decimal
    notFollowedBy (choice [char '.', char 'e', char 'E'])
    return d

{- |
Parse signed floats or integers (as floats)

>>> parseMaybe signedFloat "3"
Just 3.0

>>> parseMaybe (signedFloat :: Parser Float)  "+3"
Just 3.0

>>> parseMaybe (signedFloat :: Parser Double) "-3"
Just (-3.0)

>>> parseMaybe signedFloat "3.6E+11"
Just 3.6e11

>>> parseMaybe signedFloat "-3.6E-11"
Just (-3.6e-11)

>>> parseMaybe signedFloat "-1E99999999"
Just (-Infinity)


No space between the sign and the number:

>>> Nothing == parseMaybe signedFloat "+ 35"
True

>>> Nothing == parseMaybe signedFloat "- 35"
True

Atomic parser

>>> parseMaybe (signedFloat <|> const 0 <$> symbol "+") "+"
Just 0.0

The decimal dot is not required

>>> parseMaybe signedFloat "35"
Just 35.0

>>> parseMaybe signedFloat "3E11"
Just 3.0e11

>>> parseMaybe signedFloat "-9E+11"
Just (-9.0e11)
-}
signedFloat :: (RealFloat a) => Parser a
signedFloat = lexeme $ toRealFloat <$> L.signed (return ()) L.scientific

-- TODO: add binary
-- unsigned = char '0' ( char 'x' >> L.hexadecimal <|>  char' 'b' >> L.binary)

{- |
@setup
>>> import Data.Word
>>> import Numeric.Natural

>>> parseMaybe unsigned "11" :: Maybe Word8
Just 11

No final dot allowed:

>>> Nothing == (parseMaybe unsigned "11." :: Maybe Word8)
True

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

{-
check: https://docs.dhall-lang.org/tutorials/Language-Tour.html#multi-line-text-literals

. and Dhall also supports Nix-style multi-line string literals:

dhall
''
    #!/bin/bash

    echo "Hi!"
''
<Ctrl-D>
Text

"\n#!/bin/bash\n\necho \"Hi!\"\n"
These "double single quote strings" ignore all special characters, with one exception: if you want to include a '' in the string, you will need to escape it with a preceding ' (i.e. use ''' to insert '' into the final string).

These strings also strip leading whitespace using the same rules as Nix. Specifically: "it strips from each line a number of spaces equal to the minimal indentation of the string as a whole (disregarding the indentation of empty lines)."

You can also interpolate expressions into strings using ${...} syntax. For example:

\$ dhall
    let name = "John Doe"
in  let age  = 21
in  "My name is ${name} and my age is ${Integer/show age}"
<Ctrl-D>
Text

"My name is John Doe and my age is 21"
Note that you can only interpolate expressions of type Text

If you need to insert a "${" into a string without interpolation then use "''${" (same as Nix)

''
    for file in *; do
      echo "Found ''${file}"
    done
''
-}

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

ALT: Return a list of text lines
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