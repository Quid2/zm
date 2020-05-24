{-# LANGUAGE CPP #-}
module ZM.Type.Prims() where

import qualified ZM.Type.Words as Z
import           Data.Model
import qualified Data.Word                     as H
import qualified Data.Int                     as H
import           Numeric.Natural as H
import qualified Prelude                 as H


#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64
instance Model H.Word where envType _ = envType (Proxy::Proxy Z.Word64)
instance Model H.Int where envType _ = envType (Proxy::Proxy Z.Int64)
#elif WORD_SIZE_IN_BITS == 32
instance Model H.Word where envType _ = envType (Proxy::Proxy Z.Word32)
instance Model H.Int where envType _ = envType (Proxy::Proxy Z.Int32)
#else
#error expected WORD_SIZE_IN_BITS to be 32 or 64
#endif

instance Model H.Word8 where envType _ = envType (Proxy::Proxy Z.Word8)
instance Model H.Word16 where envType _ = envType (Proxy::Proxy Z.Word16)
instance Model H.Word32 where envType _ = envType (Proxy::Proxy Z.Word32)
instance Model H.Word64 where envType _ = envType (Proxy::Proxy Z.Word64)
instance Model H.Int8 where envType _ = envType (Proxy::Proxy Z.Int8)
instance Model H.Int16 where envType _ = envType (Proxy::Proxy Z.Int16)
instance Model H.Int32 where envType _ = envType (Proxy::Proxy Z.Int32)
instance Model H.Int64 where envType _ = envType (Proxy::Proxy Z.Int64)
instance Model H.Integer where envType _ = envType (Proxy::Proxy Z.Int)
instance Model H.Natural where envType _ = envType (Proxy::Proxy Z.Word)
