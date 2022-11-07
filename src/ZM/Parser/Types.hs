{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module ZM.Parser.Types (
    Parser,
    ADTParts (..),
    asTypeName,
    hasName,
    hasRef,
    typeNameName,
    typeNameRef,
    TypeName,
    localName,
    -- , asQualName
    Label (..),
    At,
    AtId,
    AtAbsName,
    AtError,
    Range (..),
    Val,
    ValF (..),
    pattern Constr,
    pattern VInteger,
    pattern VChar,
    pattern VString,
    pattern VFloat,
    pattern VArray,
    pattern PBind,
    pattern PWild,
    Fix (..),
    Annotate (..),
    Literal (..),
    Binder (..),
    Value,
    pattern Value,
    valType,
    valName,
    valBits,
    valFields,
    Void,
    Void1,
    module Data.Either.Validation,
) where

import Data.Either.Validation
--hiding (Value)
import Data.Fix
import Data.Semigroup (Semigroup (..))
import Data.These
import Data.Void
import Data.Word
import Text.Megaparsec hiding (Label, label)
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as P
import ZM

type Parser = Parsec Void String -- TODO: generalise to any textual type

-- |A parsed ADT
data ADTParts = ADTParts
    { name :: AtAbsName
    , vars :: [AtId]
    , constrs :: [(AtId, Fields AtId AtAbsName)]
    }
    deriving (Show)

instance Pretty ADTParts where
    pPrint p =
        pPrint (name p)
            <+> hsep (map pPrint $ vars p)
            <+> char '='
            <+> hsep (punctuate (text " |") (map pPrint (constrs p)))

--text "ADTParts" <+> pPrint (name p) <+> pPrint (vars p) <+> pPrint (constrs p)

-- | A data type name can be either local, or absolute, or both: "Bool" | "Bool.K306f1981b41c" | ".K306f1981b41c"
type TypeName l = These l AbsRef

-- TODO: Convert to These l AbsRef
-- data TypeName l =
--   TypeName l
--           (Maybe AbsRef)
--   deriving (Show, Functor, Ord, Eq)
-- localName (TypeName l _) = l
-- asQualName (TypeName l Nothing) = QualName "" "" (prettyShow l)
-- asQualName (TypeName l (Just ref)) = QualName "" (prettyShow l) (prettyShow ref)
-- instance Pretty n => Pretty (TypeName n) where
--   pPrint (TypeName n Nothing) = pPrint n
--   pPrint (TypeName n (Just r)) = pPrint n <> char '.' <> pPrint r
asTypeName :: Maybe a -> Maybe b -> These a b
asTypeName = these

typeNameName :: TypeName l -> l
typeNameName = this

typeNameRef :: TypeName l -> AbsRef
typeNameRef = that

hasName :: These a b -> Bool
hasName = hasThis

hasRef :: These a1 a -> Bool
hasRef = hasThat

localName :: These c b -> c
localName = this

instance Pretty n => Pretty (TypeName n) where
    pPrint = both pPrint (\r -> char '.' P.<> pPrint r)

-- This a == These a (any b)
data Range = Range {line, start, end :: Word32}
    deriving (Show, Eq, Ord, Generic, Flat, Model)

instance Pretty Range where
    pPrint r =
        text $
            concat
                [ "("
                , show $ line r
                , ":"
                , show $ start r
                , if end r == start r
                    then ""
                    else "-" ++ show (end r)
                , ")"
                ]

type At v = Label Range v

type AtError = At String

type AtId = At Identifier

type AtAbsName = At (TypeName Identifier)

--- 'Transparent' position-marked envelope
data Label l a = Label {label :: l, object :: a}
    deriving (Show, Functor)

instance (Pretty l, Pretty a) => Pretty (Label l a) where
    pPrint (Label l a) = pPrint a <> text "@" <> pPrint l

instance Eq p => Eq (Label l p) where
    (Label _ a) == (Label _ b) = a == b

instance Ord a => Ord (Label l a) where
    (Label _ a) <= (Label _ b) = a <= b

instance Convertible a b => Convertible (Label l a) (Label l b) where
    safeConvert (Label l a) = Label l <$> safeConvert a

instance Convertible a b => Convertible (Label l a) b where
    safeConvert (Label _ a) = safeConvert a

-- MOVE TO model
-- instance Convertible a a where safeConvert = Right

{- |A generic value (used for dynamic decoding)
 data Value =
   Value { valType :: AbsType -- ^Type
         , valName :: String -- ^Constructor name (duplicate info if we have abstype)
         , valBits :: [Bool] -- ^Bit encoding/constructor id
           -- TODO: add field names (same info present in abstype)
         , valFields :: [Value] -- ^Values to which the constructor is applied, if any
         }
   deriving (Eq, Ord, Show, NFData, Generic, Flat)
 data Void
 Value could also be expressed as
 type Value = Val (AbsType, [Bool]) Literal Void
  v0 :: [Value0]
 v0 = [lit (.., [False,True]) (PInt 3)
 |A Pattern matches a subset of values of a ZM Type
 p1 :: [Pattern]
 p1 = [lit "1:1" (PInt 3), Binder "1:2" PWild]
 type Pattern = Val () Literal Binder
 type Value = Val () Literal ()
 data Val annotation lit binder =
     Constr
       annotation
       String -- ^Name of the constructor (e.g. "True")
       (Either [Val annotation lit binder] [(String, Val annotation lit binder)]) -- ^Constructor parameters, possibly named
     --        } -- ^A standard constructor, e.g. PCon "True" []
-}

--     -- Constr { cAnnotation :: annotation
--     --        , cName :: String -- ^Name of the constructor (e.g. "True")
--     --        , cFields :: Either [Val annotation lit binder] [( String
--     --                                                             , Val annotation lit binder)] -- ^Constructor parameters
--     --        } -- ^A standard constructor, e.g. PCon "True" []
--   | lit annotation (lit (Val annotation lit binder)) -- ^A variable or a lit pattern (e.g. a string or a number)
--   | Binder annotation binder -- ^A pattern that might bind a matched value to a name
--   -- deriving (Eq, Ord, Show)
-- deriving instance (Show annotation,Show binder,Show (lit (Val annotation lit binder))) => Show (Val annotation lit binder)
-- deriving instance (Eq annotation,Eq binder,Eq (lit (Val annotation lit binder))) => Eq (Val annotation lit binder)

data Void1 a

data Annotate label f = Annotate label (f (Annotate label f))

deriving instance (Eq label, Eq (f (Annotate label f))) => Eq (Annotate label f)

deriving instance (Ord label, Ord (f (Annotate label f))) => Ord (Annotate label f)

deriving instance (Show label, Show (f (Annotate label f))) => Show (Annotate label f)

type Val lit binder = Fix (ValF lit binder)

data ValF lit binder r
    = ConstrF
        String
        -- ^Name of the constructor (e.g. "True")
        (Either [r] [(String, r)])
        -- ^Constructor parameters, possibly named
        --        } -- ^A standard constructor, e.g. PCon "True" []
    | -- |A variable or a lit pattern (e.g. a string or a number)
      LitF (lit r)
    | -- |A pattern that might bind a matched value to a name
      BinderF binder

deriving instance (Eq r, Eq binder, Eq (lit r)) => Eq (ValF lit binder r)

deriving instance (Ord r, Ord binder, Ord (lit r)) => Ord (ValF lit binder r)

deriving instance (Show r, Show binder, Show (lit r)) => Show (ValF lit binder r)

{-
type Pat = Val Literal Binder

ps :: [Pat]
ps = [PWild,PBind "a"]

y = map show ps

vs :: [Pat]
vs = [Fix (BinderF Wild),Fix (LitF (LInteger 2)),Fix (LitF (LArray [Fix (BinderF (Bind "a"))])), Fix (ConstrF "Nil" (Left [])),Fix (ConstrF "Cons" (Right [("p1",Fix (BinderF Wild))]))]

p = Constr "C" (Left [PBind "h"])

type VA label lit binder = Annotate label (ValF lit binder)
-}

pattern Constr ::
    String ->
    Either [Val lit binder] [(String, Val lit binder)] ->
    Val lit binder
pattern Constr s fs = Fix (ConstrF s fs)

pattern VInteger :: Integer -> Val Literal binder
pattern VInteger f = Fix (LitF (LInteger f))

pattern VFloat :: Double -> Val Literal binder
pattern VFloat f = Fix (LitF (LFloat f))

pattern VChar :: Char -> Val Literal binder
pattern VChar f = Fix (LitF (LChar f))

pattern VString :: String -> Val Literal binder
pattern VString f = Fix (LitF (LString f))

pattern VArray :: [Val Literal binder] -> Val Literal binder
pattern VArray f = Fix (LitF (LArray f))

pattern PWild :: Val lit Binder
pattern PWild = Fix (BinderF Wild)

pattern PBind :: String -> Val lit Binder
pattern PBind s = Fix (BinderF (Bind s))

--instance Model v => Model (Pat v)

data Literal e
    = LInteger Integer
    | LFloat Double -- Rational
    | LChar Char
    | LString String
    | LArray [e]
    deriving (Eq, Ord, Show)

data Binder
    = Wild
    | Bind String
    deriving (Eq, Ord, Show)

type Value = Annotate (AbsType, [Bool]) (ValF Literal Void)

pattern Value ::
    forall a b lit binder.
    a ->
    String ->
    b ->
    [Annotate (a, b) (ValF lit binder)] ->
    Annotate (a, b) (ValF lit binder)
pattern Value{valType, valName, valBits, valFields} = Annotate (valType, valBits) (ConstrF valName (Left valFields))

instance Pretty Value where
    pPrint :: Value -> Doc
    pPrint v =
        let hasFields = not $ null (valFields v)
            start = if hasFields then char '(' else mempty
            end = if hasFields then char ')' else mempty
         in start
                <> text (valName v)
                <> char ' '
                <> hsep (map pPrint (valFields v))
                <> end
