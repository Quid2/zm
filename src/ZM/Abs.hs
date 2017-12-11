{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

-- |Derive absolute/canonical data type models
module ZM.Abs
  ( absType
  , absTypeModel
  , absTypeModelMaybe
  , absEnv
  , absEnvWith
  , refErrors
  , kindErrors
  ) where

import           Control.Monad.Trans.RWS
import           Data.Convertible
import           Data.Foldable           (toList)
import           Data.List
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Model
import           ZM.Pretty.Base          ()
import           ZM.Types
-- import Debug.Trace

-- |Derive an absolute type for a type, or throw an error if derivation is impossible
absType :: Model a => Proxy a -> AbsType
absType = typeName . absTypeModel

-- |Derive an absolute type model for a type, or throw an error if derivation is impossible
absTypeModel :: Model a => Proxy a -> AbsTypeModel
absTypeModel = either (error . unlines) id . absTypeModelMaybe

{- |
Derive an absolute type model for a type, if possible.
-}
absTypeModelMaybe :: Model a => Proxy a -> Either Errors AbsTypeModel
absTypeModelMaybe a =
  let (TypeModel t henv) = typeModel a
  in (\(refEnv,adtEnv) -> TypeModel (solveAll refEnv t) adtEnv) <$> absEnvs henv

-- |Convert a set of relative ADTs to the equivalent ZM absolute ADTs
absEnv :: HTypeEnv -> Either Errors AbsEnv
absEnv = absEnvWith M.empty

{- |Convert a set of relative ADTs to the equivalent ZM absolute ADTs, with a starting set of known absolute ADTs.

Conversion will fail if the relative ADTs are mutually recursive or refer to undefined ADTs.
-}
absEnvWith  ::
  AbsEnv
  -> HTypeEnv
  -> Either Errors AbsEnv
absEnvWith absEnv = (snd <$>) . absEnvsWith absEnv

absEnvs :: HTypeEnv -> Either Errors (M.Map QualName AbsRef, AbsEnv)
absEnvs = absEnvsWith M.empty

absEnvsWith  ::
  AbsEnv
  -> HTypeEnv
  -> Either Errors (BiMap QualName AbsRef AbsADT)
absEnvsWith absEnv hEnv =
  let envs = fst $ execRWS (mapM_ absADT (M.keys hEnv)) extHEnv bimap

     -- It is not necessary to check for:
     -- higher kind variables as they cannot be present due to limitations in the 'model' library
     -- and for missing refs, as the compiler would not allow them

  -- Still need to check for forbidden mutual references
  -- in list (Right envs) Left (mutualErrors getHRef (trace (unwords . map prettyShow . M.keys $ extHEnv) extHEnv))
  in list (Right envs) Left (mutualErrors getHRef extHEnv)

  where
    -- add fake entries for absolute adts, to avoid missing references errors
    extHEnv = hEnv `M.union` (mmap (\(ref,adt) -> let n = qname ref adt in (n,ADT "" 0 Nothing)) absEnv)

    bimap :: BiMap QualName AbsRef AbsADT
    bimap = (mmap (\(ref,adt) -> (qname ref adt,ref)) absEnv,absEnv)

    qname ref adt = QualName "" (prettyShow ref) (convert $ declName adt)

    mmap f = M.fromList . map f . M.toList

type BuildM = RWS HTypeEnv () (BiMap QualName AbsRef AbsADT)

--type BiMap k1 k2 v = (Ord k1, Ord k2) => (M.Map k1 k2, M.Map k2 v)
type BiMap k1 k2 v = (M.Map k1 k2, M.Map k2 v)

blookup1 :: Ord k1 => k1 -> BiMap k1 k2 v -> Maybe k2
blookup1 k1 (m1,_) = M.lookup k1 m1 -- >>= \k2 -> M.lookup k2 m2

binsert :: (Ord k1, Ord k2) => k1 -> k2 -> v -> BiMap k1 k2 v -> BiMap k1 k2 v
binsert k1 k2 v (m1,m2) = (M.insert k1 k2 m1,M.insert k2 v m2)

absADT :: QualName -> BuildM AbsRef
absADT k = do
  mr <- blookup1 k <$> get
  case mr of
    Just r -> return r
    Nothing -> do
      hadt <- solve k <$> ask
      cs' <- mapM (mapM (adtRef k)) $ declCons hadt

      let adt :: AbsADT = adtNamesMap convert convert $ ADT (declName hadt) (declNumParameters hadt) cs'
      let r = absRef adt
      modify (binsert k r adt)
      return r

adtRef :: QualName -> HTypeRef -> BuildM (ADTRef AbsRef)
adtRef _ (TypVar v) = return $ Var v

adtRef me (TypRef qn) =
     if me == qn
       then return Rec
       else Ext <$> absADT qn

-- Invariants of ZM models

{-|Check that there are no mutual dependencies

> mutualErrors getADTRef absEnv
> mutualErrors getHRef hEnv
-}
mutualErrors :: (Pretty r, Ord r, Foldable t) => (a -> Maybe r) -> M.Map r (t a) -> Errors
mutualErrors getRef env = either id (map (\g-> unwords ["Found mutually recursive types",prettyShow g])) $ properMutualGroups getRef env

-- |Check that all internal references in the ADT definitions are to ADTs defined in the environment
refErrors :: AbsEnv -> Errors
refErrors env =
  let refs = nub . catMaybes . concatMap (map extRef. toList) . M.elems $ env
      definedInEnv = M.keys env
  in map (("Reference to unknown adt: " ++) . show) $ refs \\ definedInEnv
  where
    extRef (Ext ref) = Just ref
    extRef _         = Nothing

{-|
Check that all type expressions have kind *, that's to say:

    * Type constructors are fully applied
    * Type variables have * kind
-}
kindErrors :: AbsEnv -> Errors
kindErrors absEnv = (M.foldMapWithKey (\_ adt -> inContext (declName adt) $ adtTypeFold (hasHigherKind absEnv adt) adt)) absEnv
  where
    adtTypeFold :: Monoid c => (TypeN r -> c) -> ADT name1 name2 r -> c
    adtTypeFold f = maybe mempty (conTreeTypeFoldMap (foldMap f . nestedTypeNs . typeN)) . declCons

hasHigherKind :: AbsEnv -> AbsADT -> TypeN (ADTRef AbsRef) -> Errors
hasHigherKind env _ (TypeN (Ext r) rs) =
  case M.lookup r env of
    Nothing -> ["Unknown type: " ++ show r]
    Just radt -> arityCheck (convert $ declName radt) (fromIntegral (declNumParameters radt)) (length rs)

-- hasHigherKind env adt (TypeN (Var v) rs) = arityCheck adt ("parameter " ++ [varC v]) 0 (length rs)

hasHigherKind _ _ (TypeN (Var v) rs) = arityCheck ("parameter number " ++ show v) 0 (length rs)

hasHigherKind _ adt (TypeN Rec rs) =
  arityCheck
    (convert $ declName adt)
    (fromIntegral $ declNumParameters adt)
    (length rs)
arityCheck :: (Show a, Eq a) => [Char] -> a -> a -> [String]
arityCheck r expPars actPars =
     if expPars == actPars
     then []
     else [unwords ["Incorrect application of",r++",","should have",show expPars,"parameters but has",show actPars]]

list :: a -> ([t] -> a) -> [t] -> a
list onNull _      [] = onNull
list _      onList l  = onList l
