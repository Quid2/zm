-- |Generate definitions of complex types

-- Long defs
writeI8 = writeModule "Int8" $
          unwords["data Int8 = Z"
                 ,concatMap (\i -> unwords ["|",concat["N",show i],"|",concat["P",show i]]) [1..127]
                 ,"| N128"
                 ,"deriving (Eq, Ord, Read, Show, Generic)"
                 ]

w = writeW 8

writeW :: Integer -> IO ()
writeW n = do
  let nm = "Word" ++ show n
  writeModule nm $
           unwords[unwords ["data",nm,"="]
                 ,concatMap (\i -> concat [if i==0 then "" else " | ","V",show i]) [0.. 2 ^ n - 1]
                 ,"deriving (Eq, Ord, Read, Show, Generic)"
                 ]


writeL = writeModule "BlockList" $
         unwords ["data BlockList a = L0"
                           ,concatMap (\i -> unwords [concat["| L",show i],unwords . take i . repeat $ "a","(BlockList a)"]) [1..255]
                           -- ,"deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)"
                           ,"deriving (Eq, Ord, Read, Show, Generic)"
                           ]
{-
data L a = L0
         | L1 a (L a)
         | L2 a a (L a)
         | L3 a a a (L a)
-}
-- Too long and slow to compile
-- What is slow is the deriving bit (even just Generic)
-- Might generate the AbsType directly and comment out all deriving
writeModule name body =  writeFile ("/Users/titto/workspace/typed/src/QQ/"++name++".hs") $
                         unlines ["{-# LANGUAGE DeriveGeneric ,DeriveDataTypeable #-}"
                                 ,unwords ["module QQ."++name,"where"]
                                 ,"import Data.Typeable"
                                 ,"import Data.Data"
                                 ,"import GHC.Generics"
                                 ,body
                                 ]

