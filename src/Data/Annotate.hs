{-
Recursive Annotation

>>> data ArithR c = Plus c c | Minus c c | Num Int deriving (Show,Functor)
>>> type Arith = Annotate String ArithR

>>> Ann "One" (Num 1) :: Arith
Ann "One" (Num 1)

>>> Ann "Plus" $ Plus (Ann "One" $ Num 1) (Ann "Two" $ Num 2) :: Arith
Ann "sulP" (Plus (Ann "enO" (Num 1)) (Ann "owT" (Num 2)))
-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Annotate(
    Annotate,
    Ann(..),
    foldVal,
    foldAnnVal,
    annToX,
    annToF,
    F (..),
    foldF,
    fToX
) where

-- import           Data.Fix
import           Data.Bifunctor
import qualified Prettyprinter  as P

-- Example
type Arith = Ann ArithR

data ArithR c = Plus c c | Num Int deriving (Eq,Show,Functor,Foldable,Traversable)

{-
>>> P.pretty a1
Plus@Plus (One@Num 1) (Five@Plus (Two@Num 2) (Three@Num 3))

>>> P.pretty $ annToF a1
Plus (Num 1) (Plus (Num 2) (Num 3))
-}
instance (P.Pretty r,Show r) => P.Pretty (ArithR r) where
    pretty r@(Num _)    = P.viaShow r
    pretty (Plus n1 n2) = P.pretty "Plus" P.<+> P.parens (P.pretty n1) P.<+> P.parens (P.pretty n2)

a1 = Ann "Plus" $ Plus (Ann "One" $ Num 1) (Ann "Five" $ Plus (Ann "Two" $ Num 2) (Ann "Three" $ Num 3))


{-
To modify annotation use fmap:

>>> P.pretty $ fmap reverse a1
sulP@Plus (enO@Num 1) (eviF@Plus (owT@Num 2) (eerhT@Num 3))
-}

{-
To fold on values use foldVal:

>>> foldVal (\case ; Num n -> n ; Plus n1 n2 -> n1 + n2) a1
6
-}

{-
To reannotate as function of both annotation and value, use foldAnnVal:

>>> P.pretty $ eval a1
(Plus, 6)@Plus ((One, 1)@Num 1) ((Five, 5)@Plus ((Two, 2)@Num 2) (( Three
, 3 )@Num 3))
-}
eval :: Ann ArithR ann -> Ann ArithR (ann, Int)
eval = foldAnnVal go
  where
    go :: a -> ArithR (Ann ArithR (a, Int)) -> Ann ArithR (a, Int)
    go a r@(Num n)      = Ann (a,n) r
    go a r@(Plus n1 n2) = Ann (a,snd (annotation n1) + snd (annotation n2)) r

-- Code

type Annotate label f = Ann f label -- Backward compatibility

-- data Annotate label f = Ann label (f (Annotate label f))
-- deriving instance (Eq label, Eq (f (Annotate label f))) => Eq (Annotate label f)
-- deriving instance (Ord label, Ord (f (Annotate label f))) => Ord (Annotate label f)
-- deriving instance (Show label, Show (f (Annotate label f))) => Show (Annotate label f)

data Ann f ann = Ann {annotation::ann,annotated:: f (Ann f ann)} deriving (Functor,Foldable,Traversable)
deriving instance (Eq ann, Eq (f (Ann f ann))) => Eq (Ann f ann)
deriving instance (Ord ann, Ord (f (Ann f ann))) => Ord (Ann f ann)
deriving instance (Show ann, Show (f (Ann f ann))) => Show (Ann f ann)

instance (P.Pretty l, P.Pretty (f (Ann f l))) => P.Pretty (Ann f l) where
    pretty (Ann l f) = P.pretty l <> P.pretty '@' <> P.pretty f

-- Fold values (ignoring annotation)
foldVal :: Functor f => (f b -> b) -> Ann f ann -> b
foldVal f = go
    where
        go (Ann _ r) = f . fmap go $ r

-- Fold values and annotations use foldAnnVal
foldAnnVal :: Functor f => (ann -> f b -> b) -> Ann f ann -> b
foldAnnVal f = go
    where
        go (Ann a r) = f a . fmap go $ r

{-
Convert an Ann to an F

>>> annToF a1
F (Plus (F (Num 1)) (F (Plus (F (Num 2)) (F (Num 3)))))
-}
annToF :: (Functor f) => Ann f ann -> F f
annToF = annToX F

-- annToF (Ann _ r) = F (fmap annToF r)

{-
Convert an Ann to another Fix

>>> annToX F a1
F (Plus (F (Num 1)) (F (Plus (F (Num 2)) (F (Num 3)))))
-}
annToX :: Functor f => (f b -> b) -> Ann f ann -> b
annToX x (Ann _ r) = x (fmap (annToX x) r)

-- Our own Fix, to derive Show automatically
newtype F f = F (f (F f))
deriving instance (Eq (f (F f))) => Eq (F f)
deriving instance (Ord (f (F f))) => Ord (F f)
deriving instance (Show (f (F f))) => Show (F f)

{-
>>> fToX (Ann ()) $ annToF a1
Ann {annotation = (), annotated = Plus (Ann {annotation = (), annotated = Num 1}) (Ann {annotation = (), annotated = Plus (Ann {annotation = (), annotated = Num 2}) (Ann {annotation = (), annotated = Num 3})})}
-}
fToX :: Functor f => (f b -> b) -> F f -> b
fToX x (F r) = x (fmap (fToX x) r)


{-
To fold on values use foldVal:

>>> annToF a1

>>> foldF (\case ; Num n -> n ; Plus n1 n2 -> n1 + n2) $ annToF a1
6
-}
foldF :: Functor f => (f b -> b) -> F f -> b
foldF f = go
    where
        go (F r) = f . fmap go $ r



{- Pretty instance for F, hiding the presence of F

>>> P.pretty $ annToF a1
Plus (Num 1) (Plus (Num 2) (Num 3))
-}
instance (P.Pretty (f (F f))) => P.Pretty (F f) where pretty (F f) = P.pretty f


-- data A a e = A a e

-- type Exp = A () ExpR

-- e1, e2 :: Exp
-- e1 = A "" (I 4)
-- e2 = A "" (Times e1 e1)

-- data ExpR =
--       I Int
--     | Times Exp Exp
