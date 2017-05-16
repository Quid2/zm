module Data.Typed.Util(
  proxyOf
  -- *State Monad utilities
  ,runEnv
  ,execEnv
  ) where

import           Control.Monad.Trans.State
import qualified Data.Map                  as M
import           Data.Proxy

-- |Return the proxy for the type of the given value
proxyOf :: a -> Proxy a
proxyOf _ = Proxy ::Proxy a

----------- State Utils
-- |Run a State monad with an empty map as environment
runEnv :: State (M.Map k a1) a -> (a, M.Map k a1)
runEnv op = runState op M.empty

-- |Exec a State monad with an empty map as environment
execEnv :: State (M.Map k a1) a -> M.Map k a1
execEnv op = execState op M.empty

