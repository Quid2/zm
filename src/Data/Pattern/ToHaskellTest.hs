{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
module Data.Pattern.ToHaskellTest where

-- import Test.Data
-- import Test.Data.Model
-- import Test.Data.Flat
import           Data.Flat
import           Data.Model
import           Data.Pattern.ToHaskell
import           Data.Typed
-- import Network.Bot.Chat.Model
import           Data.Pattern.Types
--import Language.Haskell.TH
-- import Language.Haskell.TH.Syntax
--import qualified QQ.Pattern.ABC as ABC
import qualified Data.ByteString.Lazy   as L
--import qualified QQ.Pattern.P1 as P1
-- import Data.Typed.GHC

t = main

srcDir = "/Users/titto/workspace/quid2-qq/src"


main = do
  updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO
  pat1

-- main2 = do
--     -- r <- timeIt $ load (srcDir ++"/QQ/Pattern/Pb4a6a541b061.o") [srcDir] ["/Applications/ghc-7.10.3.app/Contents/lib/ghc-7.10.3/package.conf.d","/Users/titto/.ghc/x86_64-darwin-7.10.3/package.conf.d"] "match"
--    -- compileModule srcDir ["base","bytestring","flat","typed","quid2-qq"] "QQ.Pattern.Pb4a6a541b061"
--    -- r <- t
--    -- print r

--   r <- timeIt $ load_ (srcDir ++"/QQ/Pattern/Pb4a6a541b061.o") [srcDir] "match"
--     case r of
--       LoadFailure msg -> print msg
--       LoadSuccess m v -> print "OK"

-- srcDir = "/Users/titto/workspace/quid2-qq/src"

-- pat0 = patTst (Proxy :: Proxy N) [p|Four|] [One,Two,Three,Four,Five]
pat1 = patternTst (Proxy :: Proxy Char) [p|_|] ['a'..'z']
pat2 = patternTst (Proxy :: Proxy Char) [p|'c'|] ['a'..'z']
pat3 = patternTst (Proxy :: Proxy Message) [p|Message "john" (Subject _) _|]
   [Message "john" (Subject ["Haskell"]) Join
   ,Message "nix" (Subject ["Haskell","Meeting"]) (TextMessage "hello")
   ,Message "john" (Subject ["Haskell"]) Leave
   ]

-- |Build pattern and apply it to a set of values
patternTst :: forall a . (Show a,Model a, Flat a) => Proxy a -> Q Pat -> [a] -> IO ()
patternTst typ patt vs = do
  pat <-  patternQ patt
  patMdl <- writePattern srcDir typ pat
  compileModule srcDir ["base","bytestring","flat"] patMdl
  let bss = map encoded vs
  print bss
  -- compileModule
  mapM_ print . map (\b -> (decoded b::Decoded a)) . map Encoded . filter match $ map bytes bss
  -- mapM_ print . map (\b -> (decoded b::Decoded a)) . map Encoded $ map bytes bss

match :: L.ByteString -> Bool
match = const True

--t = modules $ absTypeEnv (Proxy :: Proxy (Forest Bool))
-- z = modules $ absTypeEnv (Proxy :: Proxy MM1)
-- a = absTypeEnv  (Proxy :: Proxy (Forest Bool))

--t1 = instanceS $ absTypeEnv  (Proxy :: Proxy (Forest Bool))

pp = putStrLn . prettyShow
