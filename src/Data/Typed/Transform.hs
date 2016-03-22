module Data.Typed.Transform where
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Typed.Types
import           Data.Foldable             (toList)
import qualified Data.Map                  as M
import           Data.Maybe

runEnv op = runState op M.empty

solve k e = case M.lookup k e of
   Nothing -> error $ unwords ["Unknown reference to",show k]
   Just v -> v

solveF :: (Functor f, Show k, Ord k) => M.Map k b -> f k -> f b
solveF env f = (\r -> solve r env) <$> f

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
