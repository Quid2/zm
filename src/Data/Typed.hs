
module Data.Typed(
  module X
  ) where

import Data.Flat as X
import Data.Model as X
import Data.Typed.Class as X
import Data.Typed.Instances as X
import Data.Typed.PrimTypes
import Data.Typed.Types as X
import Data.Typed.Value as X
import Data.Typed.Pretty as X
import Data.Typed.Transform as X

-- x = absType (Proxy::Proxy String)
-- y = absType (Proxy::Proxy (Char,Char))

