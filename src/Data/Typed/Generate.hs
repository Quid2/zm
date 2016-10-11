{-# LANGUAGE OverloadedStrings #-}
module Data.Typed.Generate where

-- import Data.Typed.Class
import Data.Typed.Types
import qualified Data.Text       as T

arrayCT = Just (asACT $ mkT_ 0 256)
asACT (P t1 t2) = ConTree (asACT t1) (asACT t2)
asACT (L 0) = Con "A0" (Left [])
asACT (L n) = let a = TypeCon $ TypVar 0
              in Con (nameConcat["A",sh n]) (Left $ replicate n a ++ [TypeApp (TypeCon (TypRef (QualName "" "" "Array"))) a])

--word8ADT = ADT {declName = "Word8", declNumParameters = 0, declCons = word8CT}
word8CT = Just (asWCT $ mkT_ 0 256)
word7CT = Just (asWCT $ mkT_ 0 128)

asWCT (P t1 t2) = ConTree (asWCT t1) (asWCT t2)
asWCT (L n) = Con (nameConcat ["V",sh n]) (Left [])

sh = name . show

data T = L Int | P T T deriving (Show) -- ,Generic)

mkT = mkT_ 1
mkT_ :: Int -> Int -> T
mkT_ p 1 = L p
mkT_ p n = let (d,m) = n `divMod` 2
           in  P (mkT_ p d) (mkT_ (p+d) (d+m))
