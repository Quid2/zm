{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typed.Types(module Data.Model.Types
                       ,AbsEnv,AbsType,AbsRef,AbsADT,RelADT,ADTRef(..),Ref(..)
                       ,NonEmptyList(..),nonEmptyList
                       ,TypedValue(..),TypedBytes(..)
                       ,proxyOf
                       ) where

import qualified Data.Map         as M
import           Data.Model.Types
import           Data.Word
import           GHC.Generics

data TypedBytes = TypedBytes AbsType (NonEmptyList Word8) deriving (Eq, Ord, Show, Generic)

data TypedValue a = TypedValue AbsType a deriving (Eq, Ord, Show, Functor, Generic)

type AbsoluteType = (AbsType,AbsEnv)

-- BUG: Possible name clash.
type AbsEnv = M.Map String (AbsRef,AbsADT)

-- Absolute Type
type AbsType = Type AbsRef

type AbsRef = Ref AbsADT

type AbsADT = NonEmptyList RelADT

type RelADT = ADT String ADTRef

data ADTRef =
  Var Word8     -- Variable
  | Rec String  -- Recursive reference, either to the type being defined or a mutually recursive type
  | Ext AbsRef -- Pointer to external definition
  deriving (Eq, Ord, Show, Generic)

data Ref a =
  Verbatim (NonEmptyList Word8) -- NO: must be explicitly padded. White padded serialisation (if required, exact bits can be recovered by decoding and recoding.. or byte padding has to be part of the definition!)
  -- | List Bit -- Express exactly the right number of bits (1/16 bits overhead per bit), useful property: adding serialised sequences without the need of decoding them.
  | Shake128 (NonEmptyList Word8)
  -- | Hash Shake128  -- A, possibly infinite sequence of bytes (useful up to 256 bit), shorter codes are prefixes of longer ones.
  deriving (Eq,Ord,Read,Show,Generic)

-- data Bytes = Bytes (NonEmptyList Word8)
-- data Shake128 = Shake128 (NonEmptyList Word8) deriving (Eq,Ord,Read,Show,Generic)

data NonEmptyList a = Elem a
                    | Cons a (NonEmptyList a)
                    deriving (Eq,Ord,Show,Read,Generic,Functor,Foldable,Traversable)

nonEmptyList :: [a] -> NonEmptyList a
nonEmptyList [] = error "Cannot convert an empty list to NonEmptyList"
nonEmptyList (h:[]) = Elem h
nonEmptyList (h:t) = Cons h (nonEmptyList t)

proxyOf :: a -> (Proxy a)
proxyOf _ = Proxy ::Proxy a
