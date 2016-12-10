module Data.Typed.Util(
  runEnv
  ,execEnv
  ,proxyOf
  ) where

import           Control.Monad.Trans.State
import qualified Data.Map                  as M
import           Data.Proxy

proxyOf :: a -> Proxy a
proxyOf _ = Proxy ::Proxy a

----------- State Utils
runEnv :: State (M.Map k a1) a -> (a, M.Map k a1)
runEnv op = runState op M.empty

execEnv :: State (M.Map k a1) a -> M.Map k a1
execEnv op = execState op M.empty

