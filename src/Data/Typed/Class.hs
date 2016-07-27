{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Typed.Class(
  Typed(..)--,absoluteType
  --,absType
  ,absTypeEnv,absADTs,absRef--,asEnv
  ,AbsEnv,AbsType
  ) where

import           Control.Monad.Reader
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Digest.Shake128
import           Data.Flat
import           Data.Int
import           Data.List
import qualified Data.Map             as M
import           Data.Model
import           Data.Ord
import           Data.Text            (Text)
-- import           Data.Typed.Generate
import           Data.Typed.Instances
import qualified Data.Typed.PrimTypes
import           Data.Typed.Transform hiding (relADT)
import           Data.Typed.Types
import           Data.Word
--import           Debug.Trace
traceShowId = id

absADTs :: Typed a => Proxy a -> [AbsADT]
absADTs = M.elems . canonicalEnv . absoluteType

--absType :: Typed a => Proxy a -> AbsType
 -- absType = fst . absoluteType

class Typed a where
  -- This can be overriden and set to a precalculated value
  absType :: Proxy a -> AbsType
  absType = canonicalType . absoluteType

  absoluteType :: Proxy a -> AbsoluteType

  default absoluteType :: Model a => Proxy a -> AbsoluteType
  absoluteType = absTypeEnv

instance {-# OVERLAPPABLE #-} Model a => Typed a where absoluteType = absTypeEnv

-- asEnv :: AbsADT -> AbsoluteType
-- asEnv adt = let r = absRef adt in (TypeCon r,M.fromList [(r,adt)])

-- TOFIX: inefficient
absTypeEnv :: Model a => Proxy a -> AbsoluteType
absTypeEnv a =
  let (t, hadts) = traceShowId $ hTypeEnv a
      henv = M.fromList $ map (\d -> (declName d, d)) hadts
      mdeps = mutualDeps . M.fromList . map (\adt -> let n = declName adt in (n,recDeps henv n)) $ hadts
      errs = filter ((>1) . length) . M.elems $ mdeps
  in if null errs
     then let qnEnv = M.fromList $ runReader (mapM (\hadt -> let qn = declName hadt in (qn,) <$> absADT qn) hadts) henv
              -- absEnv = adts . fst $ E.execRWS (mapM (absADT.declName) e) (traceShowId $ AbsRead mdeps henv) (AbsState [] M.empty)
              adtEnv = M.fromList . M.elems $ qnEnv
            in AbsoluteType adtEnv (absType qnEnv t)
       else error .
            unlines
            . map (\ms -> unwords ["Found mutually recursive types",unwords . map prettyShow $ ms])
            $ errs
  where
    absType qnEnv t = runReader (mapM absRef t) qnEnv
      where absRef (TypRef qn) = fst . solve qn <$> ask

absADT qn = do
     -- E.modify (\e -> e {adts = M.insert (locName qn) (r,absADT) (adts e)
     --                   ,contexts = tail (contexts e)
     --                   })
     hadt <- solve qn <$> ask
     cs' <- mapM (mapM (adtRef qn)) $ declCons hadt
     let adt = ADT (locName qn) (declNumParameters hadt) cs'
     return (absRef adt,adt)

adtRef _ (TypVar v) = return $ Var v

adtRef me (TypRef qn) =
     if me == qn
       then return Rec
       else Ext . fst <$> absADT qn


-- NOTE: does not detect name clashes with external (non-recursive) definitions
mutualAbsTypeEnv :: Model a => Proxy a -> (MutualAbsType, AbsEnv)
mutualAbsTypeEnv a =
  let (t, e) = hTypeEnv a
      henv = M.fromList $ map (\d -> (declName d, d)) e
      mdeps = mutualDeps . M.fromList . map (\adt -> let n = declName adt in (n,recDeps henv n)) $ e
      rdefs = sort . nub . M.elems $ mdeps
      --rdefs = gscc . mkGraph . M.toList . M.unions . map dADT $ e
      -- rdeps = M.fromList $ concatMap ((\s -> map (,s) s) . map locName) rdefs
      errs = filter (not . null . snd) . map (\g -> (g, clashes g)) $ rdefs
  in if null errs
     then let absEnv = M.fromList $ runReader (mapM (\adt ->(absName adt,) <$> mutualAbsADT (declName adt)) e) (AbsRead mdeps henv)
              -- absEnv = adts . fst $ E.execRWS (mapM (absADT.declName) e) (traceShowId $ AbsRead mdeps henv) (AbsState [] M.empty)
            in (absType absEnv t, absEnv)
       else error .
            unlines .
            map (\(g, ge) -> unwords ["In definition(s)", show g, "name clash between", show ge]) $ errs
  where
    clashes :: [QualName] -> [[QualName]]
    clashes = filter (\l -> length l > 1) . groupBy locEq . sortBy (comparing locName)

    -- absType :: AbsEnv -> HType -> AbsType
    absType absEnv t = runReader (mapM absRef t) absEnv
      where absRef (TypRef qn) = fst . solve (locName qn) <$> ask

    locEq q1 q2 = (locName q1 == locName q2) && (q1 /= q2)

type AbsM = Reader AbsRead

data AbsRead = AbsRead {
  -- ^Mutual dependencies, map from a data type and the set of its mutual dependencies
  mdeps  :: M.Map QualName [QualName]
  -- ^Haskell ADT definitions
  ,hadts :: M.Map QualName HADT
  } deriving Show

-- NOTE: inefficient, result not stored and might be recalculated multiple times in relADT
mutualAbsADT :: QualName -> AbsM (MutualAbsRef,MutualAbsADT)
mutualAbsADT qn = do
    recSet <- solve qn <$> asks mdeps
    adt <- nonEmptyList <$> mapM (relADT recSet) recSet
    let r = absRef adt
    -- E.modify (\e -> e {adts = M.insert (locName qn) (r,absADT) (adts e)
    --                   ,contexts = tail (contexts e)
    --                   })
    return (r,adt)

relADT recSet qn = do
    hadt <- solve qn <$> asks hadts
    cs' <- mapM (mapM (mutualAdtRef recSet)) $ declCons hadt
    return $ ADT (locName qn) (declNumParameters hadt) cs'

mutualAdtRef :: [QualName] -> HTypeRef -> AbsM MutualADTRef
mutualAdtRef _ (TypVar v) = return $ MVar v

mutualAdtRef recSet (TypRef qn) =
    if qn `elem` recSet
      then return $ MRec (locName qn)
      else MExt . fst <$> mutualAbsADT qn

absName :: ADT QualName ref -> Text
absName = locName . declName

absRef :: Flat a => a -> Ref a
absRef = Shake128 . nonEmptyList . B.unpack . shake128 4 . L.toStrict . flat

