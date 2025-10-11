{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module ZM.Parser.HasParser where

import Data.Comp
import Data.Comp.Ops
import Text.Megaparsec
import ZM.Parser.Lexer (lexeme)
import ZM.Parser.Types
import ZM.Parser.Util (parenthesis)

class HasParser a where
  parser :: Parser a

instance (HasParser (f a), HasParser (g a)) => HasParser ((:+:) f g a) where
  parser = (Inl <$> parser) <|> (Inr <$> parser)

-- instance {-# OVERLAPPABLE #-} (HasParser (f (Term f))) => HasParser (Term f) where
instance (HasParser (f (Term f))) => HasParser (Term f) where
  -- TODO: FIX this
  parser = Term <$> parseWithSpace parser

parseWithSpace :: Parser a -> Parser a
parseWithSpace = par . lexeme

par :: Parser a -> Parser a
par p = p <|> parenthesis (par p)
