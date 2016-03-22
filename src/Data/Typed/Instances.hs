{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
module Data.Typed.Instances where

-- import Data.Typed.Class
import Data.Model
-- import qualified Data.Model.Quid2 as Q
import Data.Typed.Types
import qualified QQ
import Data.Word
import Data.Flat

instance Model Char where envType _ = envType (Proxy::Proxy QQ.Char)
instance Model Word8 where envType _ = envType (Proxy::Proxy QQ.Word8)
instance Model Word16 where envType _ = envType (Proxy::Proxy QQ.Word16)
instance Model Word32 where envType _ = envType (Proxy::Proxy QQ.Word32)

 -- instance {-# OVERLAPPABLE #-} HasModel a => HasModel [a] where envType _ = envType (Proxy::Proxy (Q.List a))

-- instance Typed Bool where absoluteType _ = TypeCon ( Shake128 (NECons 145 (NECons 172 (NECons 19 (Elem 92)))) )

-- instance Typed String where absoluteType _ = TypeCon ( Shake128 (NECons 19 (NECons 16 (NECons 120 (Elem 36)))) )

-- instance Typed Char where absoluteType _ = TypeCon ( Shake128 (NECons 191 (NECons 40 (NECons 14 (Elem 169)))) )

-- instance Typed Q.Natural where absoluteType _ = TypeCon ( Shake128 (NECons 144 (NECons 147 (NECons 234 (Elem 17)))) )"

-- instance Typed Q.Word7 where absoluteType _ = TypeCon ( Shake128 (NECons 19 (NECons 182 (NECons 22 (Elem 112)))) )

instance (Model a,Model b) => Model (ADT a b)
instance Model a => Model (ConTree a)
instance Model a => Model (Ref a)
instance Model ADTRef
instance Model a => Model (Type a)
instance Model a => Model (TypeRef a)

data Tuple2 a b = Tuple a b deriving (Eq, Ord, Show, Generic)
instance (Model a,Model b) => Model (Tuple2 a b)

instance {-# OVERLAPPABLE #-} (Model a,Model b) => Model (a,b) where envType _ = envType (Proxy::Proxy (Tuple2 a b))

instance {-# OVERLAPPABLE #-} Model a => Model [a] where envType _ = envType (Proxy::Proxy (QQ.List a))

instance (Flat a,Flat b) => Flat (ADT a b)
instance Flat a => Flat (ConTree a)
instance Flat a => Flat (Ref a)
instance Flat ADTRef
instance Flat a => Flat (Type a)
instance Flat a => Flat (TypeRef a)
instance Flat a => Flat (NonEmptyList a)
