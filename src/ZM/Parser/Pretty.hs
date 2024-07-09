{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ZM.Parser.Pretty (pretty) where

import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import Prettyprinter
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.Terminal as Pretty.Terminal
import qualified Prettyprinter.Render.Text as Pretty.Text
import Text.Megaparsec (errorBundlePretty, parse, parseTest, runParser)
import ZM.Parser.Types

{-
>>> tt "numbers"
-}

-- then Left (unlines ["bad pretty: ", src2, "semantic was", show syntax1, "now is", show syntax2])
-- else Right $ T.pack src2

{- $setup
let p = maybe "Nothing" (show . pretty) . P.parseMaybe P.mdl
Not in scope: `P.mdl'
Not in scope: `P.parseMaybe'
-}

{-
>>> testPretty "[ 1   \n2]"
Right "[\n  1\n  2\n]"

>>> testPretty "f 1"
Right "f 1"

BAD
>>> testPretty "1+2"
Right "1 2"

>>> testPretty "1 + 2"
Right "1 + 2"

>>> testPretty "1 + f 2 / g 4"
Right "1 + f 2 / g 4"

>>> testPretty "Cons 1 Nil"
Right "Cons 1 Nil"

>>> testPretty "{\n T->F\nF -> T}"
Right "{\n  T -> F\n  F -> T\n}"

>>> testPretty "*"
Left "1:1:\n  |\n1 | *\n  | ^\nunexpected '*'\nexpecting '\"', ''', '(', '+', '-', '?', '[', '{', digit, integer, lowercase letter, or uppercase letter\n"
-}

-- instance (Pretty l, Pretty (f (Annotate l f))) => Pretty (Annotate l f) where
--   pPrint (Ann l f) = pPrint f <> chr '@' <> pPrint l

-- instance (Pretty r) => Pretty (ExpR r) where

{-
>>> p = error . either id (show . pretty) . parseMdl

>>> p "{% 11\n22\n%"
3:2:
  |
3 | %
  |  ^
unexpected end of input
expecting '}'

>>> p "{&\nx=1\ny=2\nf={T->x\nF->y\n}\nf1=f T\nf2=f F\n&} x"
{&
   x = 1
   y = 2
   f = {
            T -> x
            F -> y
   }
   f1 = f T
   f2 = f F
&} x

>>> p "{T->x\nF->{T->{X->N\nH->M\n}\nF->O\n}y\n}"
{
  T -> x
  F -> {
           T -> {
                             X -> N
                             H -> M
           }
           F -> O
  } y
}

>>>  parseMdl $ "{%\n1\n22\n%}"
Right (Ann 0 (Arr (Bracket {open = '{', close = '}', op = Just "%", values = [Ann 3 (Lit (LInteger 1)),Ann 5 (Lit (LInteger 22))]})))

-}

-- instance Pretty Char where pretty c = pretty [c]

-- Add Styles?

-- class AnnPretty a style where apretty :: a -> Doc style

-- instance AnnPretty Literal Style where apretty = scalar . pretty

-- data Style = Scalar | Symbol

-- Semantic Style
-- scalar :: Doc Style -> Doc Style
-- scalar = Pretty.annotate Scalar

-- Render Specific Style
-- scalar :: Doc AnsiStyle -> Doc AnsiStyle
-- scalar = Pretty.annotate (Pretty.Terminal.colorDull Pretty.Terminal.Magenta)
