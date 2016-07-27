{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Pattern.Types(
  Pattern(..),WildCard(..)
  ,patternQ,filterPatternQ,prefixPattern,onlyWildCards,HVar(..)
  ,Q,Pat
  ) where

import qualified Data.Flat.Bits             as V
import           Data.Foldable              (toList)
import           Data.List                  (intercalate)
import           qualified Data.Text  as T
import           Data.Typed                 hiding (Con, Val, Var)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- |A nested pattern
data Pattern v =
  -- |A constructor
  Con
  T.Text         -- ^Name of the constructor (e.g. "True")
  [Pattern v]  -- ^Patterns for the parameters

  | Var v      -- A variable

  | Val [Bool] -- A value, binary encoded (using 'flat')
  deriving (Functor,Foldable,Eq, Ord, Show, Generic)

instance Flat v => Flat (Pattern v)
instance Model v => Model (Pattern v)

-- |A Variable that can be either names (e.g. "a") or a wildcard "_"
data HVar = V String | W deriving (Eq, Ord, Show, Generic)
instance Flat HVar
instance Model HVar

-- |A wildcard "_", that matches any value
data WildCard = WildCard deriving (Eq, Ord, Show, Generic)
instance Flat WildCard
instance Model WildCard

prefixPattern :: (Foldable t, Flat a) => t a -> Pattern HVar
prefixPattern = listPatt (Var W)

listPatt :: (Foldable t, Flat a) => Pattern v -> t a -> Pattern v
listPatt = foldr (\a p -> Con "Cons" [valPattern a,p])

valPattern :: Flat a => a -> Pattern v
valPattern = Val . V.bools

-- x = filter [p|\Message _ (subject:_) _ |]
-- \subject -> Con ... v1
-- filterPatternQ :: Quasi m => Q Pat -> m Exp
filterPatternQ patq = do
     p <- convertPattern (Var . V) (Var W) patq
      -- print $ pmatch p --
     let vars = map (\(V v) -> v) . filter isVar $ toList p
     -- TODO: when done, remove haskell-src-meta
     -- let Right c = parseExp $ concat["\\",unwords $ vars,"-> onlyWildCards <$> (",showPatt p,")"]
     -- let c = concat["\\",unwords $ toList p,"->",showPatt p]
     let c = LamE (map (VarP . mkName) vars) (UInfixE (VarE (mkName "onlyWildCards")) (VarE (mkName "<$>")) (ParensE (asExp p)))
     -- print c >> print c2 >> print (c == c2)
     return c

isVar (V _) = True
isVar _ = False

patternQ :: Quasi m => Q Pat -> m (Pattern WildCard)
patternQ = convertPattern (\n -> error $ unwords ["Variables are not allowed in patterns, use wildcards (_) only, found:",n]) (Var WildCard)

-- convertPattern :: Quasi m => Q Pat -> m (Pattern String)
convertPattern
  :: Quasi m =>
     (String -> Pattern v) -> Pattern v -> Q Pat -> m (Pattern v)
convertPattern onVar onWild p = runQ (p >>= convertM onVar onWild)
  where
    convertM onVar onWild pat = case pat of
      ConP n args -> Con (T.pack $ name n) <$> mapM (convertM onVar onWild) args
      VarP n -> return $ onVar (name n)
      WildP -> return onWild
      LitP l -> return . convLit $ l
      ParensP p -> convertM onVar onWild p
      -- InfixP p1 (Name (OccName ":" ) (NameG DataName (PkgName "ghc-prim") (ModName "GHC.Types"))) p2 -> error . unwords $ ["GOTIT"]
      p -> error . unwords $ ["Unsupported pattern",pprint p]

    name (Name (OccName n) _) = n

    convLit l = case l of
       CharL c -> valPattern c
       StringL s -> valPattern s
       IntegerL i -> valPattern i -- PROB: what type to map to
       -- RationalL r -> valPattern r

showPatt :: Pattern HVar -> String
showPatt (Con n ps) = unwords ["Data.Pattern.Con",show n,"[",intercalate "," . map showPatt $ ps,"]"]
showPatt (Var (V v)) = v -- concat ["val (",v,")"] -- showVar v
 --showPatt (Var W) = "Var W" -- "WildCard" -- "WildCard" -- "_"
showPatt p = show p -- show bs -- concat [Data.BitVector,show bs

asExp (Con n ps) = AppE (AppE (c "Data.Pattern.Con") (LitE (StringL . T.unpack $ n))) (ListE (map asExp ps))
asExp (Var (V v)) = VarE (mkName v)
asExp (Var W) = AppE (c "Data.Pattern.Var") (c "W")

c = ConE . mkName

onlyWildCards W = WildCard


