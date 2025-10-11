{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module ZM.Parser.Val
  ( -- ValueF,
    Val,
    ValF (..),
    -- Binder (..),
    pattern Constr,
    pattern VInteger,
    pattern VChar,
    pattern VString,
    pattern VFloat,
    -- pattern VArray,
    Value,
    pattern Value,
    valType,
    valName,
    valBits,
    valFields,
  )
where

import Data.Aeson hiding (Value) -- (ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import Data.Annotate (F (..))
import Data.Bifunctor (bimap)
import Data.Comp
import Data.Functor.Classes (Eq1, Eq2, Show1 (liftShowsPrec), eq1, liftEq)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import GHC.IsList
import Text.PrettyPrint hiding ((<>))
import Text.Show (showListWith)
import ZM
import ZM.Parser.Literal
import ZM.Parser.Types
import ZM.Pretty

-- import qualified Text.Read.Lex as A

-- Recursive value, without attached annotations
type Val lit binder = Fix (ValF lit binder)

-- type ValueF = Fix (ValF Literal Void)

-- Result of the generic parsing of a Flat-codified value
type Value = Annotate (AbsType, [Bool]) (ValF Literal Void1)

-- instance ToJSON (ValueF) where
--     toJSON (Fix (ConstrF name (Left flds))) = A.Array . fromList $ toJSON name : map toJSON flds

-- Annotated value with type and bits
pattern Value ::
  forall a b lit binder.
  a ->
  Text ->
  b ->
  [Annotate (a, b) (ValF lit binder)] ->
  Annotate (a, b) (ValF lit binder)
pattern Value {valType, valName, valBits, valFields} = Ann (valType, valBits) (ConstrF valName (Left valFields))

pattern Constr ::
  Text ->
  Either [Val lit binder] [(Text, Val lit binder)] ->
  Val lit binder
pattern Constr s fs = Fix (ConstrF s fs)

pattern VInteger :: Integer -> Val Literal binder
pattern VInteger f = Fix (LitF (LInteger f))

pattern VFloat :: Double -> Val Literal binder
pattern VFloat f = Fix (LitF (LFloat f))

pattern VChar :: Char -> Val Literal binder
pattern VChar f = Fix (LitF (LChar f))

pattern VString :: Text -> Val Literal binder
pattern VString f = Fix (LitF (LString f))

-- pattern VArray :: [Val Literal binder] -> Val Literal binder
-- pattern VArray f = Fix (LitF (LArray f))

-- instance Model v => Model (Pat v)

-- Generic value, keep the recursion open so that we can annotate every level
data ValF lit binder r
  = ConstrF
      -- | Name of the constructor (e.g. "True")
      Text
      -- | Constructor parameters, possibly named
      --  e.g. ConstrF "True" []
      (Either [r] [(Text, r)])
  | -- | A pattern that might bind a matched value to a name
    BinderF (binder r)
  | -- | A special syntactical form, a literal (e.g. a string or a number), or a bracketed form (list,array, etc.)
    LitF lit

--  | BracketF BracketKind [ValF lit binder r]

-- deriving instance (Eq r, Eq (binder r), Eq lit) => Eq (ValF lit binder r)

-- deriving instance (Ord r, Ord binder, Ord lit) => Ord (ValF lit binder r)

deriving instance (Show r, Show (binder r), Show lit) => Show (ValF lit binder r)

deriving instance (Eq r, Eq (binder r), Eq lit) => Eq (ValF lit binder r)

-- instance (Eq1 binder, Eq lit) => Eq1 (ValF lit binder) where
--   liftEq eq (ConstrF n1 f1) (ConstrF n2 f2) =
--     n1 == n2 && case (f1, f2) of
--       (Left xs, Left ys) -> and (zipWith eq xs ys) && length xs == length ys
--       (Right xs, Right ys) -> and [t1 == t2 && eq x1 x2 | ((t1, x1), (t2, x2)) <- zip xs ys] && length xs == length ys
--       _ -> False
--   liftEq _ b1@(BinderF _) b2@(BinderF _) = b1 == b2
--   liftEq _ (LitF l1) (LitF l2) = l1 == l2
--   liftEq _ _ _ = False

-- instance (Show1 binder, Show lit) => Show1 (ValF lit binder) where
--   liftShowsPrec sp _ d (ConstrF s (Left xs)) =
--     showParen (d > 10) $
--       showString "ConstrF "
--         . showsPrec 11 s
--         . showString " (Left "
--         . showListWith (sp 0) xs
--         . showString ")"
--   liftShowsPrec sp _ d (ConstrF s (Right xs)) =
--     showParen (d > 10) $
--       showString "ConstrF "
--         . showsPrec 11 s
--         . showString " (Right "
--         . showListWith (\(t, x) -> showParen True (shows t . showString ", " . sp 0 x)) xs
--         . showString ")"
--   liftShowsPrec _ _ d (BinderF b) =
--     showParen (d > 10) $
--       showString "BinderF " . showsPrec 11 b
--   liftShowsPrec _ _ d (LitF l) =
--     showParen (d > 10) $
--       showString "LitF " . showsPrec 11 l

instance (Functor binder) => Functor (ValF lit binder) where
  fmap :: (a -> b) -> ValF lit binder a -> ValF lit binder b
  fmap f (ConstrF s fs) = ConstrF s (bimap (map f) (map (fmap f)) fs)
  fmap f (BinderF b) = BinderF (fmap f b)
  fmap _ (LitF l) = LitF l

{-
>>> pPrint (VInteger 42:: Val Literal Binder)
LInteger 42

>>> type V = Val Literal Binder
>>> pPrint ((Constr "True" (Left [])) :: V)
True

>>> pPrint (Constr "Just" (Left [Constr "True" (Left [])]) :: V)
Just True

>>> (VInteger 42:: Val Literal Binder)
Fix (LitF (LInteger 42))

>>> pPrint (VInteger 42:: Val Literal Binder)
LInteger 42
-}
instance (Pretty (f (F f))) => Pretty (F f) where pPrint (F f) = pPrint f

instance (Pretty (f (Fix f))) => Pretty (Fix f) where pPrint (Fix f) = pPrint f

-- instance (Pretty lit, Pretty (binder r), Pretty r) => Pretty (ValF lit binder r) where
--   pPrintPrec detail prec (ConstrF c (Left fs)) =
--     maybeParens (not (null fs) && prec > 0) $
--       text (unpack c)
--         <> char ' '
--         <> hsep (map (pPrintPrec detail 1) fs)
--   pPrintPrec detail prec (ConstrF c (Right fs)) =
--     maybeParens (not (null fs) && prec > 0) $
--       text (unpack c)
--         <> char '{'
--         <> hsep (punctuate (char ',') [text (unpack t) <> char '=' <> pPrintPrec detail 0 v | (t, v) <- fs])
--         <> char '}'
--   pPrintPrec detail prec (BinderF b) = pPrintPrec detail prec b
--   pPrintPrec detail prec (LitF l) = pPrintPrec detail prec l

instance Pretty Literal where pPrint l = text $ show l

{-
JSON representation of ZM values.

>>> toJSON (VInteger 42 :: ValueF)
Number 42.0

>>> toJSON (Fix (ConstrF "True" (Left [])) :: ValueF)
Array [String "True"]

>>> toJSON (Fix (ConstrF "URL" (Right [("domain", VString "google.com"), ("port", VInteger 8080)])) :: ValueF)
Array [String "URL",Object (fromList [("domain",String "google.com"),("port",Number 8080.0)])]
-}
-- data Void1 r

instance ToJSON1 (ValF Literal Void1) where
  liftToJSON _ j _ (ConstrF name (Left flds)) = A.Array . fromList $ toJSON name : map j flds
  liftToJSON _ j _ (ConstrF name (Right flds)) = A.Array . fromList $ [toJSON name, A.object (map (uncurry (.=) . bimap A.fromText j) flds)]
  liftToJSON _ _ _ (LitF (LString l)) = A.String l
  liftToJSON _ _ _ (LitF (LChar l)) = A.String . T.singleton $ l
  liftToJSON _ _ _ (LitF (LInteger l)) = A.Number (fromInteger l)
  liftToJSON _ _ _ (LitF (LFloat l)) = A.Number (realToFrac l)
  liftToJSON _ _ _ (BinderF _) = undefined

  -- FIX
  liftToEncoding = undefined -- toJSON a
