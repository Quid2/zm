module Data.Typed.Transform where
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Typed.Types
import           Data.Model.Types(fieldsTypes)
import           Data.Foldable             (toList)
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Bifunctor
import           Control.Applicative

-- Solve ADT by substituting variables and recursive refs
solvedADT :: AbsEnv -> AbsType -> ADT String AbsRef
solvedADT e at =
  let
    TypeN t ts = typeN at
    as = map typeA ts
    e' = absEnv' e
    adt = relADT $ refToADT' t e'
  in ADT (declName $ adt) 0 (conTreeTypeMap (saturateA e' as) <$> declCons adt)

relADT = head . toList

saturateA :: AbsEnv' -> [Type AbsRef] -> Type ADTRef -> Type AbsRef
saturateA e vs (TypeApp a b) = TypeApp (saturateA e vs a) (saturateA e vs b)
saturateA e vs (TypeCon (Var n)) = vs !! fromIntegral n
saturateA e vs (TypeCon (Ext k)) = TypeCon k
saturateA e vs (TypeCon (Rec s)) = TypeCon $ strToRef' s e

data AbsEnv' = AbsEnv' {envStr2Ref::M.Map String AbsRef,envRef2ADT::M.Map AbsRef AbsADT}
absEnv' e = AbsEnv' (M.map fst e) (M.fromList . M.elems $ e)

strToRef' :: String -> AbsEnv' -> AbsRef
strToRef' s (AbsEnv' sr _) = solve s sr

refToADT' :: AbsRef -> AbsEnv' -> AbsADT
refToADT' r (AbsEnv' _ ra) = solve r ra


-- |Find the code and types corresponding to a constructor
consIn :: String -> ADT name t -> Maybe ([Bool], [Type t])
consIn consName dt = maybe Nothing ((first reverse <$>) . loc []) (declCons dt)
  where
    loc bs (Con n ps) | n == consName = Just (bs,fieldsTypes ps)
                      | otherwise = Nothing
    loc bs (ConTree l r) = loc (False:bs) l <|> loc (True:bs) r

--- Mutual dependencies
mutualDeps deps = M.mapWithKey (\n ds -> filter (\o -> n `elem` (solve o deps)) ds) deps

---------- Recursive deps
-- recDeps :: M.Map QualName HADT -> QualName -> [QualName]
recDeps
   :: (Ord a, Show a, Foldable t) =>
      M.Map a (t (TypeRef a)) -> a -> [a]
recDeps hadts n = reverse $ execState (deps n) []
   where
     present n = (n `elem`) <$> get

     add n = modify (n:)

     deps n = do
       let adt = solve n hadts
       p <- present n
       unless p $ add n >> mapM_ deps (mapMaybe ref . toList $ adt)

     ref (TypRef r) = Just r
     ref (TypVar _) = Nothing

----------- Utils
runEnv op = runState op M.empty

solve :: (Ord a1, Show a1) => a1 -> M.Map a1 a -> a
solve k e = case M.lookup k e of
    Nothing -> error $ unwords ["Unknown reference to",show k]
    Just v -> v

solveF :: (Functor f, Show k, Ord k) => M.Map k b -> f k -> f b
solveF env f = (\r -> solve r env) <$> f
