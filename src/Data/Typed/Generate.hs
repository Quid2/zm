{-# LANGUAGE OverloadedStrings #-}
-- |Generate the large constructor trees of some primitive types (Array,Word8,Word7)
module Data.Typed.Generate(makeTree,arrayCT,word8CT,word7CT) where

import Data.Model.Types

-- |Constructor Tree for:
-- data Array a = A0 | A1 a (Array a) .. | A255 a .. a (Array a)
arrayCT :: Maybe (ConTree String (TypeRef QualName))
arrayCT = Just (asACT $ mkCons 256)

asACT :: Cons -> ConTree String (TypeRef QualName)
asACT (P t1 t2) = ConTree (asACT t1) (asACT t2)
asACT (L 0) = Con "A0" (Left [])
asACT (L n) = let a = TypeCon $ TypVar 0
              in Con ("A"++show n) (Left $ replicate n a ++ [TypeApp (TypeCon (TypRef (QualName "" "" "Array"))) a])

--word8ADT = ADT {declName = "Word8", declNumParameters = 0, declCons = word8CT}

-- |Constructor Tree for:
-- data Word8 = V0 | V1 .. | V255
word8CT :: Maybe (ConTree String ref)
word8CT = Just (asWCT $ mkCons 256)

-- |Constructor Tree for:
-- data Word7 = V0 | V1 .. | V127
word7CT :: Maybe (ConTree String ref)
word7CT = Just (asWCT $ mkCons 128)

asWCT :: Cons -> ConTree String ref
asWCT (P t1 t2) = ConTree (asWCT t1) (asWCT t2)
asWCT (L n) = Con (concat ["V",show n]) (Left [])

-- |A binary tree with integer leaves, used to represent constructor trees
data Cons = L Int | P Cons Cons deriving (Show)

-- |Generate a right heavier binary tree whose leaves are marked
-- with the position (starting with 0) of the corresponding constructor
-- in the list of constructors
mkCons :: Int -> Cons
mkCons = makeTree 0

makeTree :: Int -> Int -> Cons
makeTree p 1 = L p
makeTree p n = let (d,m) = n `divMod` 2
           in  P (makeTree p d) (makeTree (p+d) (d+m))
