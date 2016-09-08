{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Typed.Transform(typeDefinition,adtDefinition,mutualDeps,recDeps
                           ,stringADT,solvedADT
                           ,runEnv,execEnv,solve,solveF
                           ,label
                           ,consIn) where
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Foldable             (toList)
import           Data.List
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Model.Types          (fieldsTypes)
import           Data.Text                 (Text)
import           Data.Typed.Types

typeDefinition :: ADTEnv -> AbsType -> Either String [AbsADT]
typeDefinition env t = solveAll env . nub . concat <$> (mapM (absRecDeps env) . toList $ t)

adtDefinition :: ADTEnv -> AbsRef -> Either String [AbsADT]
adtDefinition env t = solveAll env <$> absRecDeps env t

-- solveAll env =  mapM (\k -> M.lookup k env)
solveAll env = map (flip solve env)

label env f o = (\ref -> Label ref (f <$> M.lookup ref env)) <$> o

stringADT :: ADTEnv -> AbsADT -> ADT LocalName (TypeRef LocalName)
stringADT env adt =
  let name = declName adt
  in ADT (LocalName name) (declNumParameters adt) ((solveS name <$>) <$> declCons adt)
   where solveS _ (Var n) = TypVar n
         solveS _ (Ext k) = TypRef . LocalName . declName . solve k $ env
         solveS name Rec = TypRef $ LocalName name

-- |Solve ADT by substituting variables and recursive refs
solvedADT env at =
   let
     TypeN t ts = typeN at
     as = map typeA ts
     adt = solve t env
     name = declName adt
   in ADT name 0 (conTreeTypeMap (saturate t as) <$> declCons adt)

saturate ref vs (TypeApp a b) = TypeApp (saturate ref vs a) (saturate ref vs b)
saturate _    vs (TypeCon (Var n)) = vs !! fromIntegral n
saturate _    _  (TypeCon (Ext k)) = TypeCon k
saturate ref _  (TypeCon Rec) = TypeCon ref

-- |Find the code and types corresponding to a constructor
consIn :: Text -> ADT name t -> Maybe ([Bool], [Type t])
consIn consName dt = maybe Nothing ((first reverse <$>) . loc []) (declCons dt)
  where
    loc bs (Con n ps) | n == consName = Just (bs,fieldsTypes ps)
                      | otherwise = Nothing
    loc bs (ConTree l r) = loc (False:bs) l <|> loc (True:bs) r

--- Mutual dependencies
mutualDeps :: (Ord a, Show a) => M.Map a [a] -> M.Map a [a]
mutualDeps deps = M.mapWithKey (\n ds -> filter (\o -> n `elem` (solve o deps)) ds) deps

---------- Recursive deps

-- absRecDeps :: Foldable t => M.Map AbsRef (t ADTRef) -> AbsRef -> [AbsRef]
absRecDeps env r = let (rs,errs) = recDeps__ ref id env r
                   in if null errs then Right rs else Left (unlines errs)
      where
        ref (Ext r) = Just r
        ref _ = Nothing

recDeps :: (Ord a, Show a, Foldable t) => M.Map a (t (TypeRef a)) -> a -> [a]
recDeps = recDeps_ ref id
     where
       ref (TypRef r) = Just r
       ref (TypVar _) = Nothing

recDeps_ getRef refs env n = fst $ recDeps__ getRef refs env n

recDeps__ getRef refs env n  = first reverse $ execState (deps n) ([],[])
    where
      deps n = do
         p <- present n
         unless p $ do
           add n
           case M.lookup n env of
             Nothing -> err $ unwords ["Unknown reference to",show n]
             Just v -> mapM_ deps (mapMaybe getRef . refs toList $ v)

      err e = modify (second (e:))

      present n = (n `elem`) <$> gets fst

      add n = modify (first (n:))

---------- Duplicated values
u = redup $ undup [3,5,6,3,8,5]

undup :: (Eq a, Traversable t) => t a -> t (LocalRef a)
undup t = evalState (mapM sub t) []
  where sub a = do
          env <- get
          case elemIndex a env of
            Nothing -> do
              put (a:env)
              return $ LocalDef a -- (length env) a
            --Just n -> return $ LocalRef (fromIntegral $ length env - fromIntegral (n+1))
            Just n -> return $ LocalRef (fromIntegral n)

redup :: Traversable t => t (LocalRef b) -> t b
redup t = evalState (mapM sub t) []
  where sub l = do
          env <- get
          case l of
            LocalDef a -> do
              put (a:env)
              return a
            LocalRef r -> return $ env !! fromIntegral r

----------- Utils
runEnv op = runState op M.empty
execEnv op = execState op M.empty

solve :: (Ord a1, Show a1) => a1 -> M.Map a1 a -> a
solve k e = case M.lookup k e of
     Nothing -> error $ unwords ["Unknown reference to",show k]
     Just v -> v

solveF :: (Functor f, Show k, Ord k) => M.Map k b -> f k -> f b
solveF env f = (\r -> solve r env) <$> f
