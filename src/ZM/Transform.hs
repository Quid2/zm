{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Utilities to operate on the absolute type model
module ZM.Transform (
    -- * Saturated ADTs
    MapTypeTree,
    typeTree,
    solvedADT,

    -- * Dependencies
    typeDefinition,
    adtDefinition,
    innerReferences,
    references,
) where

import Control.Monad.Trans.State
import Data.Foldable (toList)
import Data.List
import qualified Data.Map as M
import Data.Maybe
-- import           Data.Model.Util           (transitiveClosure)
import ZM.Abs
import ZM.Pretty ()
import ZM.Types
import ZM.Util

-- | A map of fully applied types to the corresponding saturated constructor tree
type MapTypeTree = M.Map (Type AbsRef) (ConTree Identifier AbsRef)

-- | Return the map of types to saturated constructor trees corresponding to the type model
typeTree :: AbsTypeModel -> MapTypeTree
typeTree tm = execEnv (addType (typeEnv tm) (typeName tm))
  where
    -- \|Insert in the env the saturated constructor trees corresponding to the passed type
    -- and any type nested in its definition
    addType env t = do
        mct <- M.lookup t <$> get
        case mct of
            Nothing ->
                case declCons $ solvedADT env t of
                    Just ct -> do
                        modify (M.insert t ct)
                        -- Recursively on all saturated types inside the contructor tree
                        mapM_ (addType env) (conTreeTypeList ct)
                    Nothing -> return ()
            Just _ -> return ()

-- | Return all the ADTs referred, directly or indirectly, by the provided type, and defined in the provided environment
typeDefinition :: AbsEnv -> AbsType -> Either [ZMError AbsRef] [AbsADT]
typeDefinition env t = mapSolve env . nub . concat <$> (mapM (absRecDeps env) . references $ t)

-- | Return all the ADTs referred, directly or indirectly, by the ADT identified by the provided reference, and defined in the provided environment
adtDefinition :: AbsEnv -> AbsRef -> Either [ZMError AbsRef] [AbsADT]
adtDefinition env t = mapSolve env <$> absRecDeps env t

-- | Return the list of references found in the ADT definition
innerReferences :: AbsADT -> [AbsRef]
innerReferences = nub . mapMaybe getADTRef . nub . toList

-- | Return the list of references found in the absolute type
references :: AbsType -> [AbsRef]
references = nub . toList

-- absRecDeps :: AbsEnv -> AbsRef -> Either String [AbsRef]
-- absRecDeps env ref = either (Left . unlines . map prettyShow) Right $ transitiveClosure getADTRef env ref
absRecDeps :: AbsEnv -> AbsRef -> Either [ZMError AbsRef] [AbsRef]
absRecDeps = transitiveClosure getADTRef

mapSolve :: (Ord k, Show k) => M.Map k b -> [k] -> [b]
mapSolve env = map (`solve` env)

-- stringADT :: AbsEnv -> AbsADT -> ADT LocalName Identifier (TypeRef LocalName)
-- stringADT env adt =
--   let name = declName adt
--   in ADT (LocalName name) (declNumParameters adt) ((solveS name <$>) <$> declCons adt)
--    where solveS _ (Var n) = TypVar n
--          solveS _ (Ext k) = TypRef . LocalName . declName . solve k $ env
--          solveS name Rec  = TypRef $ LocalName name

-- | Convert a type to an equivalent concrete ADT whose variables have been substituted by the type parameters (e.g. Maybe Bool -> Maybe = Nothing | Just Bool)
solvedADT :: (Ord ref, Show ref) => M.Map ref (ADT name consName (ADTRef ref)) -> Type ref -> ADT name consName ref
solvedADT env at =
    let
        TypeN t ts = typeN at
        as = map typeA ts
        adt = solve t env
        name = declName adt
     in
        ADT name 0 (conTreeTypeMap (saturate t as) <$> declCons adt)

-- | Substitute variables in a type with the provided types
saturate :: ref -> [Type ref] -> Type (ADTRef ref) -> Type ref
saturate ref vs (TypeApp a b) = TypeApp (saturate ref vs a) (saturate ref vs b)
saturate _ vs (TypeCon (Var n)) = vs !! fromIntegral n -- Different!
saturate _ _ (TypeCon (Ext r)) = TypeCon r
saturate selfRef _ (TypeCon Rec) = TypeCon selfRef

-- saturate2 :: ref -> [ref] -> Type (ADTRef ref) -> Type ref
-- saturate2 ref vs t = subs ref vs <$> t
--   where
--     subs _       vars (Var n) = vars !! fromIntegral n
--     subs selfRef _    Rec     = selfRef
--     subs _       _    (Ext r) = r
