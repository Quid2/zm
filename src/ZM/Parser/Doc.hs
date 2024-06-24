{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
A parser for a concise and extensible document format that can embed programming constructs (data types declarations, function, etc) as well as text, diagrams, etc.

A document is a sequence of sections, each introduced by a keyword that specify what parser is used to interpret it.

A section name should be either a sequence of characters or as sequence of symbols:

```
char 'a'

type List a = [a]

:: a declaration of same kind

test 3==2+1

adt Bool = False
    | True

{html
  <b>bold</b>
}

module X where
  id a = a
  ...
```

\| A line of text

{|
A block of text
on multiple lines.
}

{haskell

id a = a
}

Sections have two syntaxes.

Indented: a section continues till some non whitespace text appears on the first column:

name ....
  ...
  ...
  ...

Enclosed: the section name is preceded by a { and terminated by a } on the first column:

{name ...
...
...
}
Variant: name can be preceded by some optional space, needed if the name is symbolic and starts with { (needed?)

Variant: any of the three parentheses is supported () [] {}

In both cases, the text that composes the section is passed to the parser exactly as written, including the section name, but excluding the brackets and the optional space.

Space between sections is not significant.

Alternative:
For documents/notes, etc, we just use the usual data syntax.

data Note = Note {title :: String, body :: String}

Note {
  title="Parser"
  body={
  A parser for a concise and extensible document format that can embed programming constructs (data types declarations, function, etc) as well as text, diagrams, etc.}
  }
}

Document {
    elements={
    [Note "title" "body"]}
}

data Grace = Grace Text -- Grace source code

Grace {
 ...
}

## Compared

xml/html
verbose but systematic syntax

notebook format
https://docs.racket-lang.org/scribble/

markdown
/= as markdown is a sea of text with embedded subsections while here text and non-text have the same dignity.

other 'tools for thought' formats:

create a pandoc obj
https://github.com/gordonbrander/subtext/blob/main/specification.md
https://via.hypothes.is/https://talk.fission.codes/t/tools-for-thought-atjson-as-a-potential-format-for-interchange/1880
-}
module ZM.Parser.Doc where

import Control.Monad (void)
import Data.Char
import Data.Either.Extra (mapLeft)
import Data.Foldable (fold)
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import ZM.Parser.Lexer
import ZM.Parser.Types (Parser)

-- import ZM.Parser.Util (parseDoc)

{- $setup
>>> import Text.Megaparsec(parseMaybe)
-}

data Section = Section {name :: Text, body :: Text} deriving (Show)

sectionText :: Section -> Text
sectionText Section{..} = T.append name body

sectionsText :: [Section] -> Text
sectionsText = T.intercalate "\n" . map sectionText

-- multiParser :: [P b] -> Parser b
-- multiParser = foldr (\(P p c) ps -> (c <$> p) <|> ps) (fail "no parse")

-- mParser :: Monoid b => [Parser b] -> Parser b
-- mParser = fold

data P b = forall a. P {parser :: Parser a, converter :: a -> b}

-- ps = Left <$> stringLiteral

-- p1 = P stringLiteral id

-- p2 = P charLiteral (\c -> [c])

{-
Enhanced Haskell:

\* multiple modules in same file (rust style)

\* Better integration of tests and code, tests and code are written together but they get split on

module Z where

>>> sum 3 3 == 6

sum a b = a + b

-}
{-
module Portfolio where

data StkId = StkId Market String

data Stk = Stk {name::Text,id::StkId}

fn unipol = Stk "Unipol" (Bit "Uni")

table Stk -> (Stk,Money,Percent)
Unipol        https://www.google.com/finance/quote/UNI:BIT?window=1Y        BIT:UNI        22,017         46.3%

data PortfolioLine = Line Text Link Double Percent

map ()

table [unipol,bami]

-}

{- | Document evaluation

Variation: Section parsers, in addition of their content are also given access to:
* their context (the doc and their position in it)
* the parser itself so thaty can implement nested parsing.
* a read/write environment [(String,a)]

{|This is some text with an embedded graph {dot ...}
}

>>> data R = AChar Char | AStr String deriving (Show)
>>> parsers = M.fromList [("char", AChar <$> charLiteral), ("$", AStr <$> stringLiteral)]

>>> parsers = M.fromList [("char", Left <$> charLiteral), ("$", Right <$> stringLiteral)]
>>> evalSections parsers "char 'c'\n$ \"abc\""
-}
evalSections :: M.Map Text (Parser a) -> Text -> Either String [a]
evalSections ps src = mapM eval =<< parseS sections src
 where
  eval sect =
    let n = name sect
     in maybe (Left ("Unknown section " ++ T.unpack n)) (\p -> parseS p (body sect)) $ M.lookup n ps

parseS :: Parser a -> Text -> Either String a
parseS p = mapLeft errorBundlePretty . runParser p ""

{- |

>>> let src = "adt Bool = False \n | True\nadt Enum = A \n   | B\n | C" in (\ss -> src == sectionsText ss) <$> (parseMaybe sectionsDoc src)

>>> parseMaybe sectionsDoc "\n  abc \nadt Bool = False \n| True\nadt Enum = A \n   | B\n | C"
Nothing

>>> parseMaybe sectionsDoc "\n \t \r\n \nadt Bool = False \n| True\nadt Enum = A \n   | B\n | C"
-}
sections, sectionsDoc :: Parser [Section]
sectionsDoc = skipEmptyLines *> sections

{- |
>>> parseMaybe sections "-- a comment\nadt Bool = False \n | True\nadt Enum = A \n   | B\n | C"

parseMaybe sections "adt Bool = False \n | True\nadt Enum = A \n   | B\n | C"
-}
sections = many section

{- |
>>> parseMaybe section "adt"
Just (Section {name = "adt", body = ""})

>>> parseMaybe section "adt    Bool = False | True"
Just (Section {name = "adt", body = "Bool = False | True"})

>>> parseMaybe section "adt Enum = A \n   | B\n | C"
Just (Section {name = "adt", body = "Enum = A \n   | B\n | C"})
-}
section :: Parser Section
section = Section <$> sectionName <*> sectionBody

sectionBody :: Parser Text
sectionBody = T.pack <$> manyTill anyChar sectionEnd

sectionEnd :: Parser ()
sectionEnd = lookAhead (try $ void (eol >> sectionName)) <|> eof

-- parseMaybe elems "adt Enum = A \n   | B\n | C"

elems = many (Left <$> sectionName <|> Right <$> manyTill anyChar (void eol))

-- ss = (eol >> sectionName) <|> eof

-- xx ss =
--     do
--         name <-
--             (eol >> sectionName)
--         xx
--                 (name, "") : ss
--         <|> do {x <- anyChar;
--                 xs <- xx ss ; return (x : xs)

anyChar :: Parser Char
anyChar = satisfy (const True)

-- x = foldr (\n@(c:cs) st -> if isNameChar c then (n:[]) ) (False,[]) lines

-- beg (c:_) = isNameChar c

{- |
>>> parseMaybe sectionName " adt"
Nothing

>>> parseMaybe sectionName "adt"
Just "adt"

>>> parseMaybe sectionName "-()[]{}"
Nothing

>>> parseMaybe sectionName ":+*/.@#$%?><;"
Just ":+*/.@#$%?><;"

>>> parseMaybe sectionName "_"

>>> parseMaybe sectionName "{"
-}
sectionName :: Parser Text
sectionName = takeWhile1P (Just "not white space or control") isNameChar

isNameChar c = not $ isSpace c || isControl c

-- sectionName = identifier

{- |
>>> parseMaybe skipEmptyLines ""

>>> parseMaybe skipEmptyLines "\n   \t  \n"
Just ()

>>> parseMaybe skipEmptyLines "\n "
Nothing
-}
skipEmptyLines :: Parser ()
skipEmptyLines = skipMany skipEmptyLine

skipEmptyLine :: Parser ()
skipEmptyLine = hspace <* eol
