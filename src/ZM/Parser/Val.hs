{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZM.Parser.Val (
    ValueF,
    Value,
    pattern Value,
    valType,
    valName,
    valBits,
    valFields,
    Val,
    ValF (..),
    Binder (..),
    pattern Constr,
    pattern VInteger,
    pattern VChar,
    pattern VString,
    pattern VFloat,
    -- pattern VArray,
    pattern PBind,
    pattern PWild,
) where

import Data.Aeson hiding (Value) -- (ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import Data.Bifunctor (bimap)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import GHC.IsList
import Text.PrettyPrint hiding ((<>))
import ZM
import ZM.Parser.Literal
import ZM.Parser.Types
import ZM.Pretty

-- import qualified Text.Read.Lex as A

-- Result of the generic parsing of a Flat-codified value
type Value = Annotate (AbsType, [Bool]) (ValF Literal Void)

type ValueF = Fix (ValF Literal Void)

{-
JSON representation of ZM values.

>>> toJSON (Fix (BinderF undefined) :: ValueF
Prelude.undefined

>>> toJSON (Fix (ConstrF "True" (Left [])) :: ValueF)
Array [String "True"]

>>> toJSON (Fix (ConstrF "URL" (Right [("domain", VString "google.com"), ("port", VInteger 8080)])) :: ValueF)
Array [String "URL",Object (fromList [("domain",String "google.com"),("port",Number 8080.0)])]
-}

instance ToJSON1 (ValF Literal Void) where
    liftToJSON _ j _ (ConstrF name (Left flds)) = A.Array . fromList $ toJSON name : map j flds
    liftToJSON _ j _ (ConstrF name (Right flds)) = A.Array . fromList $ [toJSON name, A.object (map (uncurry (.=) . bimap A.fromText j) flds)]
    liftToJSON _ _ _ (LitF (LString l)) = A.String l
    liftToJSON _ _ _ (LitF (LChar l)) = A.String . T.singleton $ l
    liftToJSON _ _ _ (LitF (LInteger l)) = A.Number (fromInteger l)
    liftToJSON _ _ _ (LitF (LFloat l)) = A.Number (realToFrac l)
    liftToJSON _ _ _ (BinderF _) = undefined

    -- FIX
    liftToEncoding = undefined -- toJSON a

-- instance ToJSON (ValueF) where
--     toJSON (Fix (ConstrF name (Left flds))) = A.Array . fromList $ toJSON name : map toJSON flds

instance Pretty Value where
    pPrint :: Value -> Doc
    pPrint v =
        let hasFields = not $ null (valFields v)
            open = if hasFields then char '(' else mempty
            close = if hasFields then char ')' else mempty
         in open
                <> text (unpack $ valName v)
                <> char ' '
                <> hsep (map pPrint (valFields v))
                <> close

pattern Value ::
    forall a b lit binder.
    a ->
    Text ->
    b ->
    [Annotate (a, b) (ValF lit binder)] ->
    Annotate (a, b) (ValF lit binder)
pattern Value{valType, valName, valBits, valFields} = Ann (valType, valBits) (ConstrF valName (Left valFields))

type Val lit binder = Fix (ValF lit binder)

-- deriving instance (Eq binder, Eq lit) => Eq (Val lit binder)
-- deriving instance (Ord binder, Ord lit) => Ord (Val lit binder)

-- Keep the recursion open so that we can Annotate every level
data ValF lit binder r
    = ConstrF
        -- | Name of the constructor (e.g. "True")
        Text
        -- | Constructor parameters, possibly named
        --  e.g. ConstrF "True" []
        (Either [r] [(Text, r)])
    | -- | A pattern that might bind a matched value to a name
      BinderF binder
    | -- | A variable or a lit pattern (e.g. a string or a number)
      LitF lit

-- deriving (Generic1)

--  | BracketF BracketKind [ValF lit binder r]

deriving instance (Eq r, Eq binder, Eq lit) => Eq (ValF lit binder r)

deriving instance (Ord r, Ord binder, Ord lit) => Ord (ValF lit binder r)

deriving instance (Show r, Show binder, Show lit) => Show (ValF lit binder r)

instance Functor (ValF lit binder) where
    fmap :: (a -> b) -> ValF lit binder a -> ValF lit binder b
    fmap f (ConstrF s fs) = ConstrF s (bimap (map f) (map (fmap f)) fs)
    fmap _ (BinderF b) = BinderF b
    fmap _ (LitF l) = LitF l

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

pattern PWild :: Val lit Binder
pattern PWild = Fix (BinderF Wild)

pattern PBind :: Text -> Val lit Binder
pattern PBind s = Fix (BinderF (Bind s))

-- instance Model v => Model (Pat v)

data Binder
    = Wild
    | Bind Text
    deriving (Eq, Ord, Show)
