{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZM.Parser.Bracket (
    Bracket (..),
    bracket,
    prettyBracket,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prettyprinter
import qualified Prettyprinter as PP
import Text.Megaparsec
import Text.Megaparsec.Char
import ZM.Parser.Lexer
import ZM.Parser.Literal
import ZM.Parser.Types
import ZM.Parser.Util (testParse)

-- import Text.PrettyPrint (vcat)

{- List of values between brackets, space separated, with an optional operand/modifier
>>> import ZM.Parser.Lexer

>>> p = parseMaybe (bracket signedInt)

>>> p "{11 {3 4} 22}"
Nothing

>>> p "{11\n 22}"
Just (Bracket {open = '{', close = '}', op = Nothing, values = [11,22]})

NOTE: not all symbols are accepted:

>>> p "{* 11 22 *}"
Just (Bracket {open = '{', close = '}', op = Just "*", values = [11,22]})

>>> p "{ %%11 22%%}" == Nothing
True

>>> p "{%%11 22%%}"
Just (Bracket {open = '{', close = '}', op = Just "%%", values = [11,22]})

>>> p "{|11 22|}"
Just (Bracket {open = '{', close = '}', op = Just "|", values = [11,22]})

>>> p "{11 22}"
Just (Bracket {open = '{', close = '}', op = Nothing, values = [11,22]})

>>> parseMaybe (bracket (symbol "a")) "[a a]"
Just (Bracket {open = '[', close = ']', op = Nothing, values = ["a","a"]})

>>> parseMaybe (bracket (char 'a')) "[]"
Just (Bracket {open = '[', close = ']', op = Nothing, values = ""})

>>> p "[ 1 , 2 ]"
Nothing

>>> p "[,1]"
Nothing

>>> p "[1,\n]"
Nothing

>>> p "[1\n  2,3\n4 , 5\n]"
Nothing

>>> p "[\n1,2,3]"
Nothing

>>> p "[1,2,3]"
Nothing

>>> p "[ 1 \n2 \n\n]"
Just (Bracket {open = '[', close = ']', op = Nothing, values = [1,2]})

>>> p "[1 2]"
Just (Bracket {open = '[', close = ']', op = Nothing, values = [1,2]})

>>> p "[   1\n\n  2\n\n] "
Just (Bracket {open = '[', close = ']', op = Nothing, values = [1,2]})
-}
-- generate test cases
-- gen = [ T.concat ["[","]"] | n<-[0..2] ,l <- [0..2],sp <- T.take n (T.replicate " ")

bracket :: Parser e -> Parser (Bracket e)
bracket pe = lexeme $ do
    (o, c) <- choice (map (\oc@(o, _) -> oc <$ char o) bracketsOpenClose)
    msym <- optional sym
    _ <- optional wsn
    -- vs <- many (sepElem pe)
    vs <- pe `endBy` elemSep
    -- _ <- optional wsn
    _ <- maybe (string "") string msym
    _ <- char c
    return $ Bracket o c msym vs

sepElem pe =
    choice
        [ pe
        , elemSep *> pe
        ]

elemSep =
    choice
        [ -- void $ symbol "," -- This will be parser by 'expr'
          wsn
        , pure ()
        ]

bracketsOpenClose :: [(Char, Char)]
bracketsOpenClose = [('{', '}'), ('[', ']')] -- ('<','>'),('«','»')]

data Bracket e = Bracket
    { open, close :: Char
    , op :: Maybe Text
    , values :: [e]
    }
    deriving (Show, Eq, Ord, Functor)

{-
>>> p = either id (show . pretty) . testParse (bracket charLiteral)

>>> error $ p "{}"
{
}

>>> error $ p "{%% %%}"
{%%
%%}

>>> error $ p "{%% ?a ?b %%}"
{%%
     a
     b
%%}
-}
instance (Pretty e) => Pretty (Bracket e) where
    pretty = prettyBracket pretty

-- pretty (Bracket{..}) =
--     let sop = pretty . fromMaybe "" $ op
--      in pretty open
--             <> sop
--             <> column (\l -> let n = l + 1 in hardline <> indent n (vcat (map pretty values)) <> hardline)
--             <> hardlinesop
--             <> pretty close

-- ?TODO: add compact single line form {1 2 3}
prettyBracket :: (a -> Doc ann) -> Bracket a -> Doc ann
prettyBracket prettyE (Bracket{..}) =
    let sop = pretty . fromMaybe "" $ op
        beg = pretty open <> sop
        end = sop <> pretty close
     in align
            ( beg
                <> PP.space
                <> PP.space
                <> align (foldMap (\e -> hardline <> prettyE e) values)
                <> hardline
                <> end
            )

-- prettyBracket prettyE (Bracket{..}) =
--     let sop = pretty . fromMaybe "" $ op
--      in column
--             ( \s ->
--                 pretty open
--                     <> sop
--                     <> column
--                         ( \l ->
--                             hardline
--                                 <> indent (l + 2) (align (vsep (map prettyE values)))
--                                 <> hardline
--                         )
--                     <> indent s (sop <> pretty close)
--             )

-- <> hardline
-- <> PP.space
-- <> PP.space
-- <> align (vsep (map prettyE values))
-- <> hardline
-- <> sop
-- <> pretty close

-- instance (Pretty e) => Pretty (Bracket e) where
--     pPrint (Bracket open close mop vs) =
--         let op = txt . fromMaybe "" $ mop
--          in chr open <> op <> vcat (map pPrint vs) <> op <> chr close
