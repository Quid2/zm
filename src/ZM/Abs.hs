{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables    ,MultiParamTypeClasses  ,ConstraintKinds #-}
-- |Derive absolute/canonical data type models
module ZM.Abs
  ( absEnv
  , absType
  , absTypeModel
  , absTypeModelMaybe
  , relToAbsEnv
  , relToAbsEnvWith -- might not be needed
  , refErrors
  , kindErrors
  , TypeRef2(..)
  , KeyOf(..)
  , transitiveClosure
  )
where

import           Control.Monad
import           Control.Monad.Trans.RWS        ( RWS
                                                , execRWS
                                                )
import qualified Control.Monad.Trans.RWS       as RWS
import           Control.Monad.Trans.State
import           Data.Foldable                  ( toList )
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Model
import           Data.Model.Util                ( )
import           ZM.Pretty.Base                 ( )
import           ZM.Types
import           Data.List.Extra
-- import Data.Word
-- import Debug.Trace

-- $setup
-- >>> :set -XDeriveAnyClass -XDeriveGeneric
-- >>> import ZM.Pretty

type RelTypeEnv n r = TypeEnv n n (TypeRef r) r

type IsId n = Convertible n Identifier

type IsKey r = (KeyOf r (AbsRef, AbsADT), Eq r, Ord r, Show r, Pretty r)

class KeyOf k a where
  keyOf :: a -> k

instance KeyOf QualName (AbsRef,AbsADT) where
  keyOf (ref, adt) = QualName "" (convert $ declName adt) (prettyShow ref)


-- |Derive an absolute type for a type, or throw an error if derivation is impossible  
absType :: Model a => Proxy a -> AbsType
absType = typeName . absTypeModel

absEnv :: Model a => Proxy a -> AbsEnv
absEnv = typeEnv . absTypeModel

-- |Derive an absolute type model for a type, or throw an error if derivation is impossible
absTypeModel :: Model a => Proxy a -> AbsTypeModel
absTypeModel = either (error . unlines . map prettyShow) id . absTypeModelMaybe

{-|
Derive an absolute type model for a type, composed of the type plus its full type environment:

>> putStr . prettyShow $ absTypeModelMaybe (Proxy :: Proxy [Bool])
 Right Type:
<BLANKLINE>
        Kb8cd13187198 K306f1981b41c:
        List Bool
<BLANKLINE>
        Environment:
<BLANKLINE>
        Bool.K306f1981b41c ≡   False
                             | True;
<BLANKLINE>
        List.Kb8cd13187198 a ≡   Nil
                               | Cons a (List.Kb8cd13187198 a);

Provided that there are no mutual dependencies:

>>> data A = A B deriving (Model,Generic);data B = B A deriving (Model,Generic)
>>> putStr . prettyShow $ absTypeModelMaybe (Proxy :: Proxy A)
Left [Found mutually recursive types: [...B,
                                       ...A]]

and that there no higher kind types, that would be impossible anyway as is not supported by Model:

>> data Free f a = Impure (f (Free f a)) | Pure a
>> putStr . prettyShow $ absTypeModelMaybe (Proxy :: Proxy (Free Maybe Bool))

-}
absTypeModelMaybe
  :: Model a => Proxy a -> Either [ZMError QualName] AbsTypeModel
absTypeModelMaybe a =
  let (TypeModel t henv) = typeModel a
  in  (\(refEnv, adtEnv) -> TypeModel (solveAll refEnv t) adtEnv)
        <$> absEnvs henv

-- |Convert a set of relative ADTs to the equivalent ZM absolute ADTs
relToAbsEnv :: (IsKey r, IsId n) => RelTypeEnv n r -> Either [ZMError r] AbsEnv
relToAbsEnv = relToAbsEnvWith M.empty
--relToAbsEnv = (snd <$>) . absEnvs

{- |Convert a set of relative ADTs to the equivalent ZM absolute ADTs, with a starting set of known absolute ADTs.

Conversion will fail if the relative ADTs are mutually recursive or refer to undefined ADTs.
-}
relToAbsEnvWith
  :: (IsKey r, IsId n) => AbsEnv -> RelTypeEnv n r -> Either [ZMError r] AbsEnv
relToAbsEnvWith absEnv = (snd <$>) . absEnvsWith absEnv

absEnvs
  :: (IsKey r, IsId n)
  => RelTypeEnv n r
  -> Either [ZMError r] (M.Map r AbsRef, AbsEnv)
absEnvs = absEnvsWith M.empty

-- absEnvs :: (Pretty r,Eq r,Ord r,Show r, Convertible n Identifier) => RelTypeEnv n r -> Either [ZMError r]  (BiMap r AbsRef AbsADT)
--absEnvs = absEnvs__ M.empty (M.empty,M.empty)

-- absEnvs__ aEnv bimap hEnv =
--       let extEnv = hEnv `M.union` aEnv
--           env = fst $ execRWS (mapM_ absADT (M.keys hEnv)) extEnv bimap
--          -- It is not necessary to check for:
--          -- higher kind variables as they cannot be present due to limitations in the 'model' library
--          -- and for missing refs, as the compiler would not allow them
--       -- Still need to check for forbidden mutual references
--       -- in list (Right envs) Left (mutualErrors getHRef (trace (unwords . map prettyShow . M.keys $ extHEnv) extHEnv))
--       in list (Right env) Left (mutualErrors getHRef extEnv)

-- absEnvsWith :: AbsEnv -> HTypeEnv -> Either [ZMError QualName]  (BiMap QualName AbsRef AbsADT)

-- It is not necessary to check for:
--    -- higher kind variables as they cannot be present due to limitations in the 'model' library
--    -- and for missing refs, as the compiler would not allow them
-- Still need to check for forbidden mutual references

-- TO BE REMOVED

absEnvsWith
  :: (IsKey r, IsId n)
  => AbsEnv
  -> RelTypeEnv n r
  -> Either [ZMError r] (M.Map r AbsRef, AbsEnv)
absEnvsWith absEnv relEnv =
  let env = fst $ execRWS (addAbsEnv absEnv >> mapM_ absADT (M.keys relEnv))
                          relEnv
                          (M.empty, M.empty)
  --in list (Right env) Left (mutualErrors getHRef relEnv)
      --deps = (traceShowId $ M.map (mapMaybe getHRef . toList) relEnv) `M.union` (traceShowId $ M.fromList . map (\radt -> (keyOf radt,[])) .  M.toList $ absEnv)
      deps =
          (M.map (mapMaybe getHRef . toList) relEnv)
            `M.union` ( M.fromList
                      . map (\radt -> (keyOf radt, []))
                      . M.toList
                      $ absEnv
                      )
  in  list (Right env) (\h t -> Left (h : t)) (mutualErrors Just deps)

-- absEnvsWith absEnv = absEnvs__ aEnv bimap
--   -- let env = fst $ execRWS (mapM_ absADT (M.keys hEnv)) extHEnv bimap
--   -- -- in list (Right envs) Left (mutual[ZMError r]  getHRef (trace (unwords . map prettyShow . M.keys $ extHEnv) extHEnv))
--   -- in list (Right env) Left (mutual[ZMError r]  getHRef extHEnv)
--   where
--     -- |Add fake entries for absolute adts, to avoid missing references [ZMError r] 
--     aEnv =
--       --hEnv `M.union`
--       mmap
--          (\(ref, adt) ->
--             let n = qname ref adt
--             in (n, ADT "" 0 Nothing))
--          absEnv
--     bimap :: BiMap QualName AbsRef AbsADT
--     bimap = (mmap (\(ref, adt) -> (qname ref adt, ref)) absEnv, absEnv)
--     qname ref adt = QualName "" (prettyShow ref) (convert $ declName adt) -- BAD
--     mmap f = M.fromList . map f . M.toList

--qname ref adt = QualName "" (convert $ declName adt) (prettyShow ref)


-- ReaderOnlyState: k -> relADT      
-- State:
-- k -> AbsRef
-- AbsRef -> AbsADT
type BuildM n r = RWS (RelTypeEnv n r) () (BiMap r AbsRef AbsADT)

--type BiMap k1 k2 v = (Ord k1, Ord k2) => (M.Map k1 k2, M.Map k2 v)
type BiMap k1 k2 v = (M.Map k1 k2, M.Map k2 v)

-- bimap = (M.empty,M.empty)

blookup1 :: Ord k1 => k1 -> BiMap k1 k2 v -> Maybe k2
blookup1 k1 (m1, _) = M.lookup k1 m1 -- >>= \k2 -> M.lookup k2 m2

binsert :: (Ord k1, Ord k2) => k1 -> k2 -> v -> BiMap k1 k2 v -> BiMap k1 k2 v
binsert k1 k2 v (m1, m2) = (M.insert k1 k2 m1, M.insert k2 v m2)

addAbsEnv :: (IsKey r, IsId n) => AbsEnv -> BuildM n r ()
addAbsEnv = mapM_ (\radt@(ref, adt) -> addADT (keyOf radt) ref adt) . M.toList

addADT :: (IsKey r, IsId n) => r -> AbsRef -> AbsADT -> BuildM n r ()
addADT k ref adt = RWS.modify (binsert k ref adt)

absADT :: (IsKey r, IsId n) => r -> BuildM n r AbsRef
absADT k = do
  maybeAbsRef <- blookup1 k <$> RWS.get
  case maybeAbsRef of
    Just ref -> return ref
    Nothing  -> do
      relADT <- solve k <$> RWS.ask
      cs     <- mapM (mapM (adtRef k)) $ declCons relADT
      let adt :: AbsADT = adtNamesMap convert convert
            $ ADT (declName relADT) (declNumParameters relADT) cs
      let ref = absRef adt
      addADT k ref adt
      return ref

adtRef :: (IsKey r, IsId n) => r -> TypeRef r -> BuildM n r (ADTRef AbsRef)
adtRef _  (TypVar v ) = return $ Var v
adtRef me (TypRef qn) = if me == qn then return Rec else Ext <$> absADT qn

-- Invariants of ZM models
{-|Check that there are no mutual dependencies

> mutualErrors getADTRef absEnv
> mutualErrors getHRef hEnv
-}
mutualErrors
  :: (Pretty r, Ord r, Foldable t)
  => (a -> Maybe r)
  -> M.Map r (t a)
  -> [ZMError r]
mutualErrors getRef env =
  either id (map MutuallyRecursive) $ properMutualGroups getRef env

-- |Check that all internal references in the ADT definitions are to ADTs defined in the environment
-- This test is also performed as part of the relative to absolute conversion (`relToAbsEnv`)
refErrors :: (Foldable t, Eq t1) => M.Map t1 (t (ADTRef t1)) -> [ZMError t1]
refErrors env =
  let refs = nub . catMaybes . concatMap (map extRef . toList) . M.elems $ env
      definedInEnv = M.keys env
  in  map UnknownType $ refs \\ definedInEnv

extRef :: ADTRef a -> Maybe a
extRef (Ext ref) = Just ref
extRef _         = Nothing

{-|
Check that all type expressions have kind *, that's to say:

    * Type constructors are fully applied
    * Type variables have * kind
-}
--data Type
data TypeRef2 v r = Var2 v | Ext2 r

kindErrors
  :: Ord k => M.Map k (ADT name1 name2 (TypeRef2 a k)) -> [ZMError (Either a k)]
kindErrors absEnv = M.foldMapWithKey
  (\_ adt ->
        -- inContext (declName adt) $ 
             adtTypeFold (hasHigherKind2 absEnv adt) adt)
  absEnv
 where
  adtTypeFold :: Monoid c => (TypeN r -> c) -> ADT name1 name2 r -> c
  adtTypeFold f =
    maybe mempty (conTreeTypeFoldMap (foldMap f . nestedTypeNs . typeN))
      . declCons

hasHigherKind2 env _ (TypeN (Ext2 r) rs) = case M.lookup r env of
  Nothing -> [UnknownType $ Right r]
  Just radt ->
    arityCheck (Right r) -- (convert $ declName radt)
                         (fromIntegral (declNumParameters radt)) (length rs)

              -- hasHigherKind env adt (TypeN (Var v) rs) = arityCheck adt ("parameter " ++ [varC v]) 0 (length rs)
hasHigherKind2 _ _ (TypeN (Var2 v) rs) = arityCheck (Left v) 0 (length rs)

-- hasHigherKind :: AbsEnv -> AbsADT -> TypeN (ADTRef AbsRef) -> [ZMError AbsRef]
hasHigherKind env _ (TypeN (Ext r) rs) = case M.lookup r env of
  Nothing   -> [UnknownType r]
  Just radt -> arityCheck (convert $ declName radt)
                          (fromIntegral (declNumParameters radt))
                          (length rs)

        -- hasHigherKind env adt (TypeN (Var v) rs) = arityCheck adt ("parameter " ++ [varC v]) 0 (length rs)
hasHigherKind _ _   (TypeN (Var v) rs) = arityCheck v 0 (length rs)

hasHigherKind _ adt (TypeN Rec     rs) = arityCheck
  (convert $ declName adt)
  (fromIntegral $ declNumParameters adt)
  (length rs)

arityCheck :: t -> Int -> Int -> [ZMError t]
arityCheck r expPars actPars =
  if expPars == actPars then [] else [WrongKind r expPars actPars]


{-| Return the groups of mutually dependent entities, with more than one component

>>> properMutualGroups Just (M.fromList [("a",["b","c"]),("b",["a","c"]),("c",[])])
Right [["b","a"]]

-}
properMutualGroups
  :: (Ord r, Foldable t)
  => (a -> Maybe r)
  -> M.Map r (t a)
  -> Either [ZMError r] [[r]]
properMutualGroups getRef env =
  filter ((> 1) . length) <$> mutualGroups getRef env

{-| Return the groups of mutually dependent entities

>>> mutualGroups Just (M.fromList [("a",["b","c"]),("b",["a","c"]),("c",[])])
Right [["c"],["b","a"]]

-}
mutualGroups
  :: (Ord r, Foldable t)
  => (a -> Maybe r)
  -> M.Map r (t a)
  -> Either [ZMError r] [[r]]
mutualGroups getRef env = recs [] (M.keys env)
 where
  deps = transitiveClosure getRef env
  recs gs []       = return gs
  recs gs (n : ns) = do
    ds     <- deps n
    mutual <- filterM (((n `elem`) <$>) . deps) ds
    recs (mutual : gs) (ns \\ mutual)

{-| Return the transitive closure of an element in a graph of dependencies specified as an adjacency list

>>> transitiveClosure Just (M.fromList [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])]) "b"
Right ["c","a","d","b"]

>>> transitiveClosure Just (M.fromList [("a",["b","c"]),("b",["b","d","d","c"]),("c",[]),("d",["a"])]) "c"
Right ["c"]

-}
transitiveClosure
  :: (Foldable t, Ord r)
  => (a -> Maybe r)
  -> M.Map r (t a)
  -> r
  -> Either [ZMError r] [r]
transitiveClosure getRef env = execRec . deps
 where
  deps n = do
    present <- (n `elem`) <$> gets seen
    unless present $ do
      modify (\st -> st { seen = n : seen st })
      case M.lookup n env of
        Nothing ->
          modify (\st -> st { errors = UnknownType n -- unwords ["transitiveClosure:Unknown reference to", prettyShow n]
                                                     : errors st })
        Just v -> mapM_ deps (mapMaybe getRef . toList $ v)

execRec :: State (RecState r) a -> Either [ZMError r] [r]
execRec op =
  (\st -> if null (errors st) then Right (seen st) else Left (errors st))
    $ execState op (RecState [] [])

data RecState r = RecState {seen::[r],errors::[ZMError r]} deriving Show

