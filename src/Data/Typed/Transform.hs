{-# LANGUAGE NoMonomorphismRestriction #-}
-- |Utilities to operate on the absolute type model
module Data.Typed.Transform(
  typeDefinition,adtDefinition
  ,MapTypeTree,typeTree
  -- |*State utilities
  -- ,recDeps
  ,stringADT
  ,solvedADT
  ,innerReferences,references
  ) where

import           Control.Monad.Trans.State
import           Data.Foldable             (toList)
import           Data.List
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Model.Util(dependencies)
import           Data.Typed.Types
import           Data.Typed.Util
-- -- import Data.BLOB
-- import Data.Flat

-- typeDefinition :: AbsTypeModel -> Either String [AbsADT]
-- typeDefinition (TypeModel t env) = mapSolve env . nub . concat <$> (mapM (absRecDeps env) . references $ t)

-- |A map of saturated types to the corresponding saturated constructor tree
type MapTypeTree = M.Map (Type AbsRef) (ConTree Identifier AbsRef)

typeTree :: AbsTypeModel -> MapTypeTree
typeTree tm = execEnv (addType (typeEnv tm) (typeName tm))
 where
   -- |Insert in the env the saturated constructor trees corresponding to the passed type
   -- and any type nested in its definition
   addType absEnv t = do
     mct <- M.lookup t <$> get
     case mct of
       Nothing ->
         case declCons $ solvedADT absEnv t of
           Just ct -> do
             modify (M.insert t ct)
             -- Recursively on all saturated types inside the contructor tree
             mapM_ (addType absEnv) (conTreeTypeList ct)
           Nothing -> return ()
       Just _ -> return ()

typeDefinition :: AbsEnv -> AbsType -> Either String [AbsADT]
typeDefinition env t = mapSolve env . nub . concat <$> (mapM (absRecDeps env) . references $ t)

adtDefinition :: AbsEnv -> AbsRef -> Either String [AbsADT]
adtDefinition env t = mapSolve env <$> absRecDeps env t

absRecDeps :: AbsEnv -> AbsRef -> Either String [AbsRef]
absRecDeps env ref = either (Left . unlines) Right $ dependencies getADTRef env ref

mapSolve :: (Ord k, Show k) => M.Map k b -> [k] -> [b]
mapSolve env = map (`solve` env)

-- stringADT :: AbsEnv -> AbsADT -> ADT LocalName Identifier (TypeRef LocalName)
-- stringADT env adt =
--   let name = declName adt
--   in ADT (LocalName name) (declNumParameters adt) ((solveS name <$>) <$> declCons adt)
--    where solveS _ (Var n) = TypVar n
--          solveS _ (Ext k) = TypRef . LocalName . declName . solve k $ env
--          solveS name Rec  = TypRef $ LocalName name

stringADT :: AbsEnv -> AbsADT -> ADT Identifier Identifier (TypeRef Identifier)
stringADT env adt =
  let name = declName adt
  in ADT name (declNumParameters adt) ((solveS name <$>) <$> declCons adt)
   where solveS _ (Var n) = TypVar n
         solveS _ (Ext k) = TypRef . declName . solve k $ env
         solveS name Rec  = TypRef name

-- |Solve ADT by substituting variables and recursive refs
solvedADT
  :: (Ord ref, Show ref) =>
     M.Map ref (ADT name consName (ADTRef ref))
     -> Type ref -> ADT name consName ref
solvedADT env at =
   let
     TypeN t ts = typeN at
     as = map typeA ts
     adt = solve t env
     name = declName adt
   in ADT name 0 (conTreeTypeMap (saturate t as) <$> declCons adt)
--      where -- PROB!

-- |Substitute variables in type with the
saturate :: ref -> [Type ref] -> Type (ADTRef ref) -> Type ref
saturate ref vs (TypeApp a b) = TypeApp (saturate ref vs a) (saturate ref vs b)
saturate _   vs (TypeCon (Var n)) = vs !! fromIntegral n -- Different!
saturate _    _  (TypeCon (Ext r)) = TypeCon r
saturate selfRef _  (TypeCon Rec) = TypeCon selfRef

saturate2 :: ref -> [ref] -> Type (ADTRef ref) -> Type ref
saturate2 ref vs t = subs ref vs <$> t
  where
    subs _       vs (Var n) = vs !! fromIntegral n
    subs selfRef vs Rec     = selfRef
    subs _       _  (Ext r) = r

---------- ADT dependencies
-- |Mutual dependencies, for every entry the list of mutual dependencies
-- >>>mutualDeps (M.fromList [("a",["b","c"]),("b",["a","c"]),("c",[])])
-- fromList [("a",["b"]),("b",["a"]),("c",[])]
-- mutualDeps :: (Ord a, Show a) => M.Map a [a] -> M.Map a [a]
-- mutualDeps deps = M.mapWithKey (\n ds -> filter (\o -> n `elem` (solve o deps)) ds) deps

innerReferences = nub . catMaybes . map getADTRef . references

references = nub . toList



-- |Direct and indirect references from an adt to other adts
-- recDeps = dependencies getHRef


