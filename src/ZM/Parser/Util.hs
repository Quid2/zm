{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ZM.Parser.Util (
  -- * Parsing 
  parseDoc,
  -- , parseE
  syntaxError, -- PUBLIC?

  -- * Position Handling
  mkAt,
  at,

  -- * Parser transformers
  doc,
  parenthesis,
  cpars,
  spars,
) where

import Data.Bifunctor (Bifunctor (first))
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec hiding (Label)
import ZM.Parser.Lexer 
import ZM.Pretty
import ZM.Parser.Types (
  Label (Label),
  Parser,
  RangeLine(..),
 )

{- $setup
 >>> import ZM.Parser.Lexer(float)
-}

{- | Parse a string using the provided parser
parseDoc :: Parser a -> String -> Either AtError a
-}
parseDoc parser = parseE (doc parser)

-- parseE :: Parser a -> String -> Either AtError a
parseE :: (TraversableStream s, VisualStream s, ShowErrorComponent e) => Parsec e s c -> s -> Either (Label RangeLine String) c
parseE p = first syntaxError . parse p ""

#if MIN_VERSION_megaparsec(9,0,0)
syntaxError :: (TraversableStream s, VisualStream s,ShowErrorComponent e) => ParseErrorBundle s e -> Label RangeLine String

#elif MIN_VERSION_megaparsec(8,0,0)
syntaxError :: (Stream s, ShowErrorComponent e) => ParseErrorBundle s e -> AtError

#elif MIN_VERSION_megaparsec(7,0,0)
syntaxError :: (ShowErrorComponent e, Stream s) => ParseErrorBundle s e -> AtError
#endif

#if MIN_VERSION_megaparsec(8,0,0)
syntaxError errs =
  let
      msg  = unwords . lines . parseErrorTextPretty $ err
      err  = NE.head . bundleErrors $ errs
      (_, pst') = reachOffset (errorOffset err) (bundlePosState errs)
      pos = pstateSourcePos pst'
  in  mkAt pos 1 msg

#elif MIN_VERSION_megaparsec(7,0,0)

-- NOTE: for 7 and 8 might also parse the output of 'errorBundlePretty'
syntaxError errs =
  let
      msg  = unwords . lines . parseErrorTextPretty $ err
      err  = NE.head . bundleErrors $ errs
      (pos, _, _) = reachOffset (errorOffset err) (bundlePosState errs)
  in  mkAt pos 1 msg

#else

syntaxError :: (Ord t, ShowErrorComponent e, ShowToken t) => ParseError t e -> AtError
syntaxError err =
  let pos = NE.head $ errorPos err
  in  mkAt pos 1 (unwords $ tail $ lines $ parseErrorPretty err)

#endif

{- |
Make the parser into a document parser (that will parse any initial space and till eof)
-}
doc :: Parser a -> Parser a
doc = between wsn (wsn >> eof)
-- doc = between ws eof

{- |
Parses something between square parenthesis "[..]"

>>> parseMaybe (spars float) "[3.7 ]"
Just 3.7
-}
spars :: Parser a -> Parser a
spars = between (symbol "[") (symbol "]")

{- |
Parses something between curly parenthesis "{..}"

>>> parseMaybe (cpars float) "{ 3.7 }"
Just 3.7

>>> parseMaybe (cpars float) "{ 3.7 -- a number\n} -- curly stuff"
Just 3.7
-}
cpars :: Parser a -> Parser a
cpars = between (symbol "{") (symbol "}")

{- | Parses something between parenthesis "(..)"

>>> parseMaybe (parenthesis float) "()"
Nothing

>>> parseMaybe (parenthesis float) "( 3.7)"
Just 3.7
-}
parenthesis :: Parser a -> Parser a
parenthesis = between (symbol "(") (symbol ")")


-- Position

{- Add location information to the result of a parser.

We assume that:
\* the parser does not parse initial space
\* the length of parsed text is equal to the length of the pretty-shown result
\* the parsed text is disposed on a single line
-}

at :: (TraversableStream s, MonadParsec e s m, Pretty a2) => m a2 -> m (Label RangeLine a2)
at parser = do
  pos <- getSourcePos
  r <- parser
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


mkAt :: SourcePos -> Int -> a -> Label RangeLine a
mkAt pos len = Label (mkRange pos len)

mkRange :: (Integral a) => SourcePos -> a -> RangeLine
mkRange pos len =
  let asP f = (\n -> n - 1) . fromIntegral . unPos . f
      st = asP sourceColumn pos
   in RangeLine (asP sourceLine pos) st (st + fromIntegral len - 1)

