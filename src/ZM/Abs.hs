{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

-- |Derive absolute/canonical data type models
module ZM.Abs
  ( absType
  , absTypeModel
  , absTypeModelMaybe
  , absEnv
  , refErrors
  , kindErrors
  ) where

import           "mtl" Control.Monad.Reader
import           Data.Bifunctor
import           Data.Convertible
import           Data.Foldable              (toList)
import           Data.List
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Model
import           ZM.Types

-- |Derive an absolute type for a type, or throw an error if derivation is impossible
absType :: Model a => Proxy a -> AbsType
absType = typeName . absTypeModel

-- |Derive an absolute type model for a type, or throw an error if derivation is impossible
absTypeModel :: Model a => Proxy a -> AbsTypeModel
absTypeModel = either (error . unlines) id . absTypeModelMaybe

{- |
Derive an absolute type model for a type, provided that:

* is an instance of Model
* no data type referred directly or indirectly by the type:

    * has higher kind variables
    * is mutually recursive with other data types
-}
absTypeModelMaybe :: Model a => Proxy a -> Either Errors AbsTypeModel
absTypeModelMaybe a =
  let (TypeModel t henv) = typeModel a
  in (\(refEnv,adtEnv) -> TypeModel (solveAll refEnv t) adtEnv) <$> absEnvs henv

absEnv :: M.Map QualName (ADT String String (TypeRef QualName)) -> Either Errors AbsEnv
absEnv = (snd <$>) . absEnvs

absEnvs :: M.Map QualName (ADT String String (TypeRef QualName)) -> Either [String] (M.Map QualName AbsRef, M.Map AbsRef AbsADT)
absEnvs henv =
  let envs = second M.fromList . first M.fromList . unzip $ runReader (mapM (\n -> (\m -> ((n,fst m),m)) <$> absADT n) (M.keys henv)) henv

     -- It is not necessary to check for:
     -- higher kind variables as they cannot be present due to limitations in the 'model' library
     -- and for missing refs, as the compiler would not allow them

  -- Still need to check for forbidden mutual references
  in const envs <$> (properMutualGroups getHRef henv >>= checkMutualErrors)
  where
    checkMutualErrors mgroups =
      if null mgroups
      then Right ()
      else Left $ map (\g-> unwords ["Found mutually recursive types",prettyShow g]) mgroups

absADT :: QualName -> Reader HTypeEnv (AbsRef, AbsADT)
absADT qn = do
     hadt <- solve qn <$> ask
     cs' <- mapM (mapM (adtRef qn)) $ declCons hadt

     let adt :: AbsADT = adtNamesMap convert convert $ ADT (declName hadt) (declNumParameters hadt) cs'
     return (absRef adt,adt)

adtRef :: QualName -> HTypeRef -> Reader HTypeEnv (ADTRef AbsRef)
adtRef _ (TypVar v) = return $ Var v

adtRef me (TypRef qn) =
     if me == qn
       then return Rec
       else Ext . fst <$> absADT qn

-- Invariants of ZM models

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
Check that all type expressions have kind *:
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
