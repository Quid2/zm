-- |Model for mutually recursive data 
module Data.Timeless.Mutual where
import Data.Typed.Types
import Data.Word

-- Mutual recursive data types

-- Test
data Forest a = Forest (List (Tree a))

data Tree a = Tree a (Forest a)

data List a = Cons a (List a)
            | Nil

type MutualADTEnv = [(MutualAbsRef,MutualAbsADT)]

type MutualAbsType = Type MutualAbsRef

-- newtype or type? 
newtype MutualAbsRef = MutualAbsRef (SHA3_256_6 MutualAbsADT)

type MutualAbsADT = ADT Identifier Identifier (MutualADTRef AbsRef)

data MutualADTRef a =
   Var Word8   -- Variable
   | Rec String  -- Recursive reference, either to the type being defined or a mutually recursive type
   | Ext MutualAbsRef -- Pointer to external definition


-- Single type

data SingleRef a = SingleRef

type SingleType = Type (SingleRef Boolean)

data Boolean = And Boolean Boolean | Or Boolean Boolean | Not Boolean | True | False


-- Alternative Hash
