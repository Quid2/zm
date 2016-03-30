{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections,UndecidableInstances       #-}
module Data.Typed.Class(
  Typed(..)--,absoluteType
  ,absType,absTypeEnv,absADTs,absRef
  ,AbsEnv,AbsType
  ) where

import           Control.Monad.Reader
import           Data.Typed.Instances
import           Data.Typed.Transform hiding (relADT)
import           Data.Typed.Types
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as L
import           Data.Digest.Shake128
import           Data.Flat
import           Data.List
import qualified Data.Map                    as M
import           Data.Model
import           Data.Ord

absADTs :: Typed a => Proxy a -> [AbsADT]
absADTs = map snd . M.elems . snd . absoluteType

absType :: Typed a => Proxy a -> AbsType
absType = fst . absoluteType

class Typed a where
  absoluteType :: Proxy a -> AbsoluteType

  -- default absoluteType :: Model a => Proxy a -> AbsType
  -- absoluteType = absType

instance Model a => Typed a where
  absoluteType = absTypeEnv

absTypeEnv :: Model a => Proxy a -> (AbsType, AbsEnv)
absTypeEnv a =
  let (t, e) = hTypeEnv a
      henv = M.fromList $ map (\d -> (declName d, d)) e
      mdeps = mutualDeps . M.fromList . map (\adt -> let n = declName adt in (n,recDeps henv n)) $ e
      rdefs = sort . nub . M.elems $ mdeps
      --rdefs = gscc . mkGraph . M.toList . M.unions . map dADT $ e
      -- rdeps = M.fromList $ concatMap ((\s -> map (,s) s) . map locName) rdefs
      errs = filter (not . null . snd) . map (\g -> (g, clashes g)) $ rdefs
  in if null errs
     then let absEnv = M.fromList $ runReader (mapM (\adt ->(absName adt,) <$> absADT (declName adt)) e) (AbsRead mdeps henv)
              -- absEnv = adts . fst $ E.execRWS (mapM (absADT.declName) e) (traceShowId $ AbsRead mdeps henv) (AbsState [] M.empty)
            in (absType absEnv t, absEnv)
       else error .
            unlines .
            map (\(g, ge) -> unwords ["In definition(s)", show g, "name clash between", show ge]) $ errs
  where
    clashes :: [QualName] -> [[QualName]]
    clashes = filter (\l -> length l > 1) . groupBy locEq . sortBy (comparing locName)

    absType :: AbsEnv -> HType -> AbsType
    absType absEnv t = runReader (mapM absRef t) absEnv
      where absRef (TypRef qn) = fst . solve (locName qn) <$> ask

    locEq q1 q2 = (locName q1 == locName q2) && (q1 /= q2)

type AbsM = Reader AbsRead

data AbsRead = AbsRead {
  mdeps  :: M.Map QualName [QualName]
  ,hadts :: M.Map QualName HADT
  } deriving Show

hadtGet :: QualName -> AbsM HADT
hadtGet qn = solve qn <$> asks hadts

absADT :: QualName -> AbsM (AbsRef,AbsADT)
absADT qn = do
   rs <- solve qn <$> asks mdeps
   absADT <- nonEmptyList <$> mapM (relADT rs) rs
   let r = absRef absADT
   -- E.modify (\e -> e {adts = M.insert (locName qn) (r,absADT) (adts e)
   --                   ,contexts = tail (contexts e)
   --                   })
   return (r,absADT)

relADT rs qn = do
   let name = locName qn
   hadt <- hadtGet qn
   cs' <- mapM (mapM (adtRef rs)) $ declCons hadt
   return $ ADT name (declNumParameters hadt) cs'

-- adtRef :: HTypeRef -> AbsM ADTRef
adtRef rs (TypVar v) = return $ Var v

adtRef rs (TypRef qn) = do
    -- r <- qn `elem` rs -- isRecDef qn
   if qn `elem` rs
     then return $ Rec (locName qn)
     else Ext . fst <$> absADT qn

absName :: ADT QualName ref -> String
absName = locName . declName

absRef :: Flat a => a -> Ref a
absRef = Shake128 . nonEmptyList . B.unpack . shake128 4 . L.toStrict . flat
