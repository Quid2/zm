{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- |Derive absolute/canonical data type models
module Data.Typed.Abs (absType,absTypeModel,absTypeModelMaybe) where

import           "mtl" Control.Monad.Reader
import qualified Data.ListLike.String as L
import qualified Data.Map             as M
import           Data.Model
import           Data.Typed.Types

-- |Derive an absolute type for a type, or throw an error if derivation is impossible
absType :: Model a => Proxy a -> AbsType
absType = typeName . absTypeModel

-- |Derive an absolute type model for a type, or throw an error if derivation is impossible
absTypeModel :: Model a => Proxy a -> AbsTypeModel
absTypeModel = either error id . absTypeModelMaybe

{- |
Derive an absolute type model for a type, provided that:

* is an instance of Model
* no data type referred directly or indirectly by the type:

    * has higher kind variables
    * is mutually recursive with other data types
-}
absTypeModelMaybe :: Model a => Proxy a -> Either String AbsTypeModel
absTypeModelMaybe a =
  let (TypeModel t henv) = typeModel a
      names = M.keys henv

      -- TODO: Check for higher kind variables (currently not required as they cannot be present due to limitations in the 'model' library)

      -- Check for forbidden mutual references
      errs = filter ((> 1) . length) $ mutualGroups getHRef henv
  in if null errs
       then
       -- convert environment and type to absolute form
       let adtEnv :: [(AbsRef, AbsADT)]
           adtEnv = runReader (mapM absADT names) henv

           qnEnv :: M.Map QualName AbsRef
           qnEnv = M.fromList $ zip names (map fst adtEnv)
       in Right (TypeModel (solveAll qnEnv t) (M.fromList adtEnv))
       else Left .
            unlines
            . map (\ms -> unwords ["Found mutually recursive types", unwords . map prettyShow $ ms]) $ errs

absADT :: QualName -> Reader HTypeEnv (AbsRef, AbsADT)
absADT qn = do
     hadt <- solve qn <$> ask
     cs' <- mapM (mapM (adtRef qn)) $ declCons hadt

     let adt :: AbsADT = adtNamesMap L.fromString L.fromString $ ADT (declName hadt) (declNumParameters hadt) cs'
     return (absRef adt,adt)

adtRef :: QualName -> HTypeRef -> Reader HTypeEnv (ADTRef AbsRef)
adtRef _ (TypVar v) = return $ Var v

adtRef me (TypRef qn) =
     if me == qn
       then return Rec
       else Ext . fst <$> absADT qn
