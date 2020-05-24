{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction,CPP #-}

module ZM.Parser.Util
  ( parseDoc
  -- , parseE
  , mkAt
  , syntaxError -- PUBLIC?
      -- * Parser transformers
  , doc
  , pars
  , cpars
  , spars
  )
where

-- import           Data.Model
-- import           Data.Void
-- import           Data.Word
import           Text.Megaparsec         hiding ( Label )
-- import           Text.Megaparsec.Error   hiding ( Label )
import           ZM.Parser.Lexer
import           ZM.Parser.Types
-- import           ZM.To.Util
-- import           ZM.Types
-- import           ZM.Util
import qualified Data.List.NonEmpty            as NE
import           Data.Bifunctor

-- |Parse a string using the provided parser
parseDoc :: Parser a -> String -> Either AtError a
parseDoc parser = parseE (doc parser)

parseE :: Parser a -> String -> Either AtError a
parseE p = first syntaxError . parse p ""

#if MIN_VERSION_megaparsec(8,0,0)
syntaxError :: (Stream s, ShowErrorComponent e) => ParseErrorBundle s e -> AtError
syntaxError errs =      
  let 
      msg  = unwords . lines . parseErrorTextPretty $ err
      err  = NE.head . bundleErrors $ errs
      (_, pst') = reachOffset (errorOffset err) (bundlePosState errs)
      pos = pstateSourcePos pst'
  in  mkAt pos 1 msg

#elif MIN_VERSION_megaparsec(7,0,0)

-- NOTE: for 7 and 8 might also parse the output of 'errorBundlePretty'

syntaxError
  :: (ShowErrorComponent e, Stream s)
  => ParseErrorBundle s e
  -> AtError
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
     

mkAt :: SourcePos -> Int -> a2 -> Label Range a2
mkAt pos len = Label (mkRange pos len)

mkRange :: Integral a => SourcePos -> a -> Range
mkRange pos len =
  let asP f = (\n -> n - 1) . fromIntegral . unPos . f
      st = asP sourceColumn pos
  in  Range (asP sourceLine pos) st (st + fromIntegral len - 1)

{-|
Make the parser into a document parser (that will parse any initial space and till eof)
-}
doc :: Parser a -> Parser a
doc = between sc eof

{-| 
Parses something between square parenthesis "[..]"

>>> parseMaybe (spars float) "[3.7 ]"
Just 3.7
-}
spars :: Parser a -> Parser a
spars = between (symbol "[") (symbol "]")

{-| 
Parses something between curly parenthesis "{..}"

>>> parseMaybe (cpars float) "{ 3.7 }"
Just 3.7

>>> parseMaybe (cpars float) "{ 3.7 -- a number\n} -- curly stuff"
Just 3.7
-}
cpars :: Parser a -> Parser a
cpars = between (symbol "{") (symbol "}")

{-| Parses something between parenthesis "(..)"

>>> parseMaybe (pars float) "()"
Nothing

>>> parseMaybe (pars float) "( 3.7 )"
Just 3.7
-}
pars :: Parser a -> Parser a
pars = between (symbol "(") (symbol ")")
