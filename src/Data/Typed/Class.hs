{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Data.Typed.Class(
  Typed(..)
  --,absoluteType,absType
  ,absTypeEnvBy,absADTs--,absRef--,asEnv
  -- ,refK
  ,refS
  ,AbsType
  ) where

import           "mtl" Control.Monad.Reader
import           Data.Bifunctor       (first, second)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Digest.SHA3
import           Data.Flat
import           Data.Int
import           Data.List
import qualified Data.ListLike.String as L
import qualified Data.Map             as M
import           Data.Model
import           Data.Ord
import           Data.Text            (Text)
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

  -- Is this needed?
  default absoluteType :: Model a => Proxy a -> AbsoluteType
  absoluteType = absTypeEnv

-- Is this needed?
instance {-# OVERLAPPABLE #-} Model a => Typed a where absoluteType = absTypeEnv

-- asEnv :: AbsADT -> AbsoluteType
-- asEnv adt = let r = absRef adt in (TypeCon r,M.fromList [(r,adt)])
-- zz = absTypeEnvBy refS

-- TOFIX: extraordinarily inefficient
-- absTypeEnv :: Model a => Proxy a -> AbsoluteType
absTypeEnv a = let (e,t) = absTypeEnvBy refS a in AbsoluteType e t

-- absTypeEnvBy :: (AsType (Ana a),Flat a) => (HADT -> AbsRef) -> Proxy a -> (ADTEnv,AbsType)
absTypeEnvBy ref a =
  let (t, hadts) = traceShowId $ hTypeEnv a
      -- hadts = absNames <$> hadts0
  -- let (t, hadts) = traceShowId $ second (map (adtNamesMap (L.fromString . declName) L.fromString)) $ hTypeEnv a
      henv = M.fromList $ map (\d -> (declName d, d)) hadts
      mdeps = mutualDeps . M.fromList . map (\adt -> let n = declName adt in (n,recDeps henv n)) $ hadts
      errs = filter ((>1) . length) . M.elems $ mdeps
  in if null errs
     then let qnEnv = M.fromList $ runReader (mapM (\hadt -> let qn = declName hadt in (qn,) <$> absADT ref qn) hadts) henv
              -- adtEnv = M.map (adtNamesMap L.fromString L.fromString) . M.fromList . M.elems $ qnEnv
              adtEnv = M.fromList . M.elems $ qnEnv
              --adtEnv = M.fromList . M.elems $ qnEnv
          in (adtEnv,absType qnEnv t)
       else error .
            unlines
            . map (\ms -> unwords ["Found mutually recursive types",unwords . map prettyShow $ ms])
            $ errs
  where
    absType qnEnv t = runReader (mapM absSolve t) qnEnv
      where absSolve (TypRef qn) = fst . solve qn <$> ask
    -- adtName = L.fromString . declName

absNames = adtNamesMap L.fromString L.fromString

absADT ref qn = do
     -- E.modify (\e -> e {adts = M.insert (locName qn) (r,absADT) (adts e)
     --                   ,contexts = tail (contexts e)
     --                   })
     hadt <- solve qn <$> ask
     cs' <- mapM (mapM (adtRef ref qn)) $ declCons hadt
     -- let adt = ADT (locName qn) (declNumParameters hadt) cs'
     let adt :: AbsADT = absNames $ ADT (locName qn) (declNumParameters hadt) cs'
     return (ref adt,adt)

adtRef _ _ (TypVar v) = return $ Var v

adtRef ref me (TypRef qn) =
     if me == qn
       then return Rec
       else Ext . fst <$> absADT ref qn

type AbsM = Reader AbsRead

data AbsRead = AbsRead {
  -- ^Mutual dependencies, map from a data type and the set of its mutual dependencies
  mdeps  :: M.Map QualName [QualName]
  -- ^Haskell ADT definitions
  ,hadts :: M.Map QualName HADT
  } deriving Show

--absName :: ADT QualName ref -> Name
absName = locName . declName

-- absRef :: Flat a => a -> Ref a
-- absRef = Shake128 . nonEmptyList . B.unpack . shake128 4 . L.toStrict . flat


-- refS :: AbsADT -> AbsRef
-- refS :: ADT Name (ADTRef SHA3_256_6) -> SHA3_256_6
-- refK :: Flat a => a -> SHA3_256_6
--refS a = let (w1:w2:w3:w4:w5:w6:[]) = B.unpack . sha3_256 6 . L.toStrict . flat $ a
refS a = let ([w1,w2,w3,w4,w5,w6]) = B.unpack . sha3_256 6 . L.toStrict . flat $ a
         in AbsRef $ SHA3_256_6 w1 w2 w3 w4 w5 w6

-- refK :: ADT Text (ADTRef Keccak256_6) -> Keccak256_6
-- -- refK :: Flat a => a -> Keccak256_6
-- refK a = let (w1:w2:w3:w4:w5:w6:_) = B.unpack . keccak256_6 . L.toStrict . flat $ a
--            in Keccak256_6 w1 w2 w3 w4 w5 w6

-- refS a = let (w1:w2:w3:w4:_) = B.unpack . keccak256_6 . L.toStrict . flat $ a
--            in Shake128_4 w1 w2 w3 w4
