{- Additional List functions -}
module Data.List.Extra (list) where

{-| from the 'extra' package    
>>> list 1 (\v _ -> v - 2) []
1

>>> list 1 (\v _ -> v - 2) [5,6,7]
3
-}
list :: b -> (a -> [a] -> b) -> [a] -> b
list nil _    [] = nil
list _   cons (x:xs) = cons x xs