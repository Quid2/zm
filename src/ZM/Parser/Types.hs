{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module ZM.Parser.Types (
    Parser,
    asTypeName,
    hasName,
    hasRef,
    -- typeNameName,
    -- typeNameRef,
    TypeName,
    localTypeName,
    absTypeName,
    -- , asQualName
    Label (..),
    At,
    AtId,
    AtAbsName,
    AtError,
    Offset,
    Range (..)
    ,RangeLine (..),
    Fix (..),
    F(..),
    Annotate (..),
    unAnn,
    Void,
    Void1,
    module Data.Either.Validation,
) where

import Data.Either.Validation
import Data.Fix
import Data.Functor.Classes
import Data.Text (Text)
import Data.These.Extra
import Data.Void
import Data.Word
import Text.Megaparsec hiding (Label, label)
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as P
import Text.Show.Deriving
import ZM

type Parser = Parsec Void Text

type Offset = Int

{- | A data type name can be either local, or absolute, or both: "Bool" | ".K306f1981b41c" | "Bool.K306f1981b41c"

>>> asTypeName (Just "Bool") Nothing
This "Bool"

>>> pPrint $ asTypeName (Just "Bool") Nothing
"Bool"
-}

-- n = asTypeName "Bool" (asbType :: Proxy Bool)

type TypeName l = These l AbsRef

asTypeName :: Maybe s -> Maybe AbsRef -> TypeName s
asTypeName = aThese

hasName :: TypeName s -> Bool
hasName = hasThis

hasRef :: TypeName s -> Bool
hasRef = hasThat

localTypeName :: TypeName s -> s
localTypeName = fromThis

absTypeName :: TypeName s -> AbsRef
absTypeName = fromThat

instance (Pretty n) => Pretty (TypeName n) where
    pPrint = both pPrint (\r -> char '.' P.<> pPrint r)

{-
>>> pPrint $ Range 3 7 11
(3:7-11)
-}
data RangeLine = RangeLine {line, startPos, endPos :: Word32}     deriving (Show, Eq, Ord, Generic, Flat, Model)

instance Pretty RangeLine where
    pPrint (RangeLine {..}) =
        text $
            concat
                [ "("
                , show line
                , ":"
                , show startPos
                , if endPos == startPos
                    then ""
                    else "-" ++ show endPos
                , ")"
                ]

data Range = Range {start, end :: Word32} deriving (Show, Eq, Ord, Generic, Flat, Model)

instance Pretty Range where
    pPrint (Range {..}) =
        text $
            concat
                [ "("
                , show start
                , if end == start
                    then ""
                    else "-" ++ show end
                , ")"
                ]

type At v = Label RangeLine v

type AtError = At String

type AtId = At Identifier

type AtAbsName = At (TypeName Identifier)

{- 'Transparent' labeled envelope

>>> pPrint $ Label (Range 3 7 11) "Bool"
"Bool"@(3:7-11)

Label is transparent with respect to equality and ordering (why?):

>>> Label (Range 3 7 11) "Bool" == Label (Range 2 3 5) "Bool"
True
-}
data Label l a = Label {label :: l, object :: a}
    deriving (Show, Functor)

instance (Pretty l, Pretty a) => Pretty (Label l a) where
    pPrint (Label l a) = pPrint a <> text "@" <> pPrint l

instance (Eq p) => Eq (Label l p) where
    (Label _ a) == (Label _ b) = a == b

instance (Ord a) => Ord (Label l a) where
    (Label _ a) <= (Label _ b) = a <= b

instance (Convertible a b) => Convertible (Label l a) (Label l b) where
    safeConvert (Label l a) = Label l <$> safeConvert a

instance (Convertible a b) => Convertible (Label l a) b where
    safeConvert (Label _ a) = safeConvert a

-- MOVE TO model
-- instance Convertible a a where safeConvert = Right

{- | A generic value (used for dynamic decoding)
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

{-

>>> ConstrF "True" (Left []) :: Val Literal Void
Couldn't match type: ValF
                       lit0_a6FLu[tau:1] binder0_a6FLv[tau:1] r0_a6FLw[tau:1]
               with: Fix (ValF Literal Void)
Expected: Val Literal Void
  Actual: ValF
            lit0_a6FLu[tau:1] binder0_a6FLv[tau:1] r0_a6FLw[tau:1]
In the expression: ConstrF "True" (Left []) :: Val Literal Void
In an equation for `it_a6FID':
    it_a6FID = ConstrF "True" (Left []) :: Val Literal Void

-}
-- instance Pretty (Val lit binder) where
--     pPrint v =
--         let hasFields = not $ null (valFields v)
--             start = if hasFields then char '(' else mempty
--             end = if hasFields then char ')' else mempty
--          in start
--                 <> text (valName v)
--                 <> char ' '
--                 <> hsep (map pPrint (valFields v))
--                 <> end

{-
Recursive Annotation

>>> Ann "Bool" $ ConstrF "True" (Left []) :: Annotate String (ValF Literal Void)
Ann "Bool" (ConstrF "True" (Left []))

>>> Ann "Bool" $ LitF (LChar 't') :: Annotate String (ValF Literal Void)
Ann "Bool" (LitF (LChar 't'))

>>> Ann "Bool" $ LitF (LArray [Ann "Char" $ LitF (LChar 't')]) :: Annotate String (ValF Literal Void)
Ann "Bool" (LitF (LArray [Ann "Char" (LitF (LChar 't'))]))

>>> Ann (Range 1 3 4) $ BinderF Wild :: Annotate Range (ValF Void1 Binder)
Ann (Range {line = 1, start = 3, end = 4}) (BinderF Wild)

>>> Ann "Bool" (LitF (LChar 't')) == (Ann "Bool" $ LitF (LChar 't') :: Annotate String (ValF Literal Void))
True
-}
data Annotate label f = Ann label (f (Annotate label f))

deriving instance (Eq label, Eq (f (Annotate label f))) => Eq (Annotate label f)
deriving instance (Ord label, Ord (f (Annotate label f))) => Ord (Annotate label f)
deriving instance (Show label, Show (f (Annotate label f))) => Show (Annotate label f)

unAnn :: Functor f => Annotate label f -> F f
unAnn (Ann _ r) = F (fmap unAnn r)

-- Our own Fix, to derive Show automatically.
newtype F f = F (f (F f))
deriving instance (Eq (f (F f))) =>  Eq (F f)
deriving instance (Ord (f (F f))) =>  Ord (F f)
deriving instance (Show (f (F f))) =>  Show (F f)

-- Pretty instance for F, hiding the presence of F
instance (Pretty (f (F f))) => Pretty (F f) where pPrint (F r) = pPrint r

-- instance (Show (f (F f))) =>  Show (F f) where
--     show (F r) = show r


{-
>>> ConstrF "True" (Left []) :: Val Literal Void

>>> show (VChar 't' :: Val Literal Void)
-}
-- s :: String
-- s =  show (VChar 't' :: Val Literal Void)

-- ee = (VChar 't' :: Val Literal Void) == (VChar 't' :: Val Literal Void)

-- ee = (VChar 't' :: Val Literal Void) == (VChar 't' :: Val Literal Void)

{-
type Pat = Val Literal Binder

ps :: [Pat]
ps = [PWild,PBind "a"]

y = map show ps

vs :: [Pat]
vs = [Fix (BinderF Wild),Fix (LitF (LInteger 2)),Fix (LitF (LArray [Fix (BinderF (Bind "a"))])), Fix (ConstrF "Nil" (Left [])),Fix (ConstrF "Cons" (Right [("p1",Fix (BinderF Wild))]))]

p = Constr "C" (Left [PBind "h"])

type VA label lit binder = Annotate label (ValF lit binder)

>>> show $ (VString "abc" :: Pat)
Not in scope: type constructor or class `Pat'
-}

{-
>>> VChar 't' :: Val Literal Void
No instance for (Show1 (ValF Literal Void))
  arising from a use of `evalPrint'
In a stmt of an interactive GHCi command: evalPrint it_akHm1
-}

data Void1 a

instance Show (Void1 a) where show _ = ""

-- newtype V = ValF Literal Binder

-- $(deriveShow1 ''V)
