{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Mapping of basic Haskell types to equivalent ZhengMing types (Char, (), Words, Ints, Floats, Text, Tuples, List, Seq, Map)
module ZM.Model () where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import qualified Data.ByteString.Short   as SBS
import           Flat               (UTF16Text, UTF8Text)
--import qualified Data.Int                as H
import qualified Data.Map                as M
import           Data.Model
import qualified Data.Sequence           as S
import           Data.Text               (Text)
-- import           ZM.Type.Array
import qualified ZM.Type.BLOB    as Z
import qualified ZM.Type.Char    as Z
import           ZM.Type.Float32
import           ZM.Type.Float64
import           ZM.Type.List
import qualified ZM.Type.Map     as Z
import           ZM.Type.Tuples
import           ZM.Type.Unit
-- import qualified ZM.Type.Words   as Z
-- import qualified Data.Word               as H
-- import           Numeric.Natural as H
import qualified Prelude                 as H
import           Type.Analyse
import ZM.Type.Array
import ZM.Type.Prims()

#include "MachDeps.h"

instance Model H.Char where envType _ = envType (Proxy::Proxy Z.Char)

instance Model () where envType _ = envType (Proxy::Proxy Unit)

-- Signed and Unsigned Whole Numbers

-- #if WORD_SIZE_IN_BITS == 64
-- instance Model H.Word where envType _ = envType (Proxy::Proxy Z.Word64)
-- instance Model H.Int where envType _ = envType (Proxy::Proxy Z.Int64)
-- #elif WORD_SIZE_IN_BITS == 32
-- instance Model H.Word where envType _ = envType (Proxy::Proxy Z.Word32)
-- instance Model H.Int where envType _ = envType (Proxy::Proxy Z.Int32)
-- #else
-- #error expected WORD_SIZE_IN_BITS to be 32 or 64
-- #endif

-- instance Model H.Word8 where envType _ = envType (Proxy::Proxy Z.Word8)
-- instance Model H.Word16 where envType _ = envType (Proxy::Proxy Z.Word16)
-- instance Model H.Word32 where envType _ = envType (Proxy::Proxy Z.Word32)
-- instance Model H.Word64 where envType _ = envType (Proxy::Proxy Z.Word64)
-- instance Model H.Int8 where envType _ = envType (Proxy::Proxy Z.Int8)
-- instance Model H.Int16 where envType _ = envType (Proxy::Proxy Z.Int16)
-- instance Model H.Int32 where envType _ = envType (Proxy::Proxy Z.Int32)
-- instance Model H.Int64 where envType _ = envType (Proxy::Proxy Z.Int64)
-- instance Model H.Integer where envType _ = envType (Proxy::Proxy Z.Int)
-- instance Model H.Natural where envType _ = envType (Proxy::Proxy Z.Word)

-- Floating-Point Numbers
instance Model H.Float where envType _ = envType (Proxy::Proxy IEEE_754_binary32)
instance Model H.Double where envType _ = envType (Proxy::Proxy IEEE_754_binary64)

-- Data Structures
instance Model a => Model [a] where envType _ = envType (Proxy::Proxy (List a))

instance Model a => Model (S.Seq a) where envType _ = envType (Proxy::Proxy (List a))
-- Changed in flat 0.4.*?
-- instance Model a => Model (S.Seq a) where envType _ = envType (Proxy::Proxy (Array a))

instance (Model a,Model b) => Model (M.Map a b) where envType _ = envType (Proxy::Proxy (Z.Map a b))
-- instance {-# OVERLAPPING #-} (AsType a, AsType b) => AsType (App (App (Typ (Map A0 A1)) a) b) where asType _ = asType (undefined::(App (Typ [A0]) (App (App (Typ (A0, A1)) a) b)))


-- ByteStrings
instance Model B.ByteString where envType _ = envType (Proxy::Proxy Bytes)
instance Model L.ByteString where envType _ = envType (Proxy::Proxy Bytes)
instance Model SBS.ShortByteString where envType _ = envType (Proxy::Proxy Bytes)

-- Texts

--  NOTE: When the kind of the type for which we define the model does not match that of the type we are mapping to we also need to define an AsType instance
instance Model Text where envType _ = envType (Proxy::Proxy (Z.BLOB Z.UTF8Encoding))
instance {-# OVERLAPPING #-} AsType (Typ Text) where asType _ = asType (H.undefined::Ana (Z.BLOB Z.UTF8Encoding))

instance Model UTF8Text where envType _ = envType (Proxy::Proxy (Z.BLOB Z.UTF8Encoding))
instance {-# OVERLAPPING #-} AsType (Typ UTF8Text) where asType _ = asType (H.undefined::Ana (Z.BLOB Z.UTF8Encoding))

instance Model UTF16Text where envType _ = envType (Proxy::Proxy (Z.BLOB Z.UTF16LEEncoding))
instance {-# OVERLAPPING #-} AsType (Typ UTF16Text) where asType _ = asType (H.undefined::Ana (Z.BLOB Z.UTF16LEEncoding))

-- Tuples
instance (Model a,Model b) => Model (a,b) where envType _ = envType (Proxy::Proxy (Tuple2 a b))

instance (Model a,Model b,Model c) => Model (a,b,c) where envType _ = envType (Proxy::Proxy (Tuple3 a b c))

instance (Model a,Model b,Model c,Model d) => Model (a,b,c,d) where envType _ = envType (Proxy::Proxy (Tuple4 a b c d))

instance (Model a1,Model a2,Model a3,Model a4,Model a5) => Model (a1,a2,a3,a4,a5) where envType _ = envType (Proxy::Proxy (Tuple5 a1 a2 a3 a4 a5))

instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6) => Model (a1,a2,a3,a4,a5,a6) where envType _ = envType (Proxy::Proxy (Tuple6 a1 a2 a3 a4 a5 a6))

instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7) => Model (a1,a2,a3,a4,a5,a6,a7) where envType _ = envType (Proxy::Proxy (Tuple7 a1 a2 a3 a4 a5 a6 a7))

instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7,Model a8) => Model (a1,a2,a3,a4,a5,a6,a7,a8) where envType _ = envType (Proxy::Proxy (Tuple8 a1 a2 a3 a4 a5 a6 a7 a8))

instance (Model a1,Model a2,Model a3,Model a4,Model a5,Model a6,Model a7,Model a8,Model a9) => Model (a1,a2,a3,a4,a5,a6,a7,a8,a9) where envType _ = envType (Proxy::Proxy (Tuple9 a1 a2 a3 a4 a5 a6 a7 a8 a9))

