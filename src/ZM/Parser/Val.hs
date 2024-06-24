{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module ZM.Parser.Val (
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

import Data.Text (Text, unpack)
import Text.PrettyPrint hiding ((<>))
import ZM
import ZM.Parser.Literal
import ZM.Parser.Types
import ZM.Pretty

-- Result of the generic parsing of a Flat-codified value
type Value = Annotate (AbsType, [Bool]) (ValF Literal Void)

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
    | -- \|A variable or a lit pattern (e.g. a string or a number)

      -- | BracketF BracketKind [ValF lit binder r]
      LitF lit

deriving instance (Eq r, Eq binder, Eq lit) => Eq (ValF lit binder r)

deriving instance (Ord r, Ord binder, Ord lit) => Ord (ValF lit binder r)

deriving instance (Show r, Show binder, Show lit) => Show (ValF lit binder r)

-- type Val lit binder = Fix (ValF lit binder)

-- deriving instance (Eq binder, Eq lit) => Eq (Val lit binder)
-- deriving instance (Ord binder, Ord lit) => Ord (Val lit binder)

-- -- Keep the recursion open so that we can Annotate every level
-- data ValF lit binder r
--     = ConstrF
--         String
--         -- ^Name of the constructor (e.g. "True")
--         (Either [r] [(String, r)])
--         -- ^Constructor parameters, possibly named
--         -- e.g. ConstrF "True" []

--     | -- |A pattern that might bind a matched value to a name
--       BinderF binder

--     -- | BracketF BracketKind [ValF lit binder r]

--     | -- |A variable or a lit pattern (e.g. a string or a number)
--       LitF lit

-- deriving instance (Eq r, Eq binder, Eq lit) => Eq (ValF lit binder r)

-- deriving instance (Ord r, Ord binder, Ord lit) => Ord (ValF lit binder r)

-- deriving instance (Show r, Show binder, Show lit) => Show (ValF lit binder r)

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
