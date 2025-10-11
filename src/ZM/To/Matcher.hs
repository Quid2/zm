{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module ZM.To.Matcher
  ( dWild,
    BTree (..),
    -- typeMatcherMap,
    -- MapTypeMatcher,
    -- PatternMatcher,
    -- Match (..),
    -- patternMatcher,
  )
where

import Data.Int
import qualified Data.Map as M
import Data.Word
import Flat
import Flat.Decoder
import ZM hiding (Name, Var)

-- $setup
-- >>> :set -XDeriveAnyClass -XDeriveGeneric

-- type PatternMatcher = (TypeMatchers,[Match AbsRef [BitMask]])

-- | A map of types to the corresponding wildcard matcher
-- type MapTypeMatcher = M.Map AbsType BTree

-- | A form of constructors tree suitable for quick pattern matching
--  |Corresponds to a wildcard, it matches any value of a given type
data BTree
  = -- | Constructors' tree
    BFork BTree BTree
  | -- | Skip a number of bits
    Skip
      Int
  | -- | Constructor parameters
    -- BTyp [AbsType]
    BTyp
      AbsType
  | BSeq [BTree]
  deriving (Show, Eq)

isSimple :: BTree -> Bool
isSimple (Skip _) = True
isSimple (BTyp _) = True
isSimple (BSeq []) = True
-- isSimple (BSeq [_]) = True
isSimple _ = False

simpler :: (AbsType, M.Map AbsType BTree) -> BTree -> BTree
simpler tm (BFork l r) = simplerFork (simpler tm l) (simpler tm r)
simpler tm (BSeq xs) = simplerSeq $ map (simpler tm) xs
simpler env@(ctx, tm) (BTyp t) =
  let st = solve t tm
   in if ctx /= t && isSimple st
        then simpler env st
        else BTyp t
simpler _ e = e

simplerFork :: BTree -> BTree -> BTree
simplerFork (BSeq []) (BSeq []) = Skip 1
simplerFork (Skip n1) (Skip n2) | n1 == n2 = Skip (n1 + 1)
simplerFork l r = BFork l r

simplerSeq :: [BTree] -> BTree
simplerSeq xs = case noskip xs of
  [o] -> o
  ys -> BSeq ys

noskip :: [BTree] -> [BTree]
noskip (Skip n : xs) = skip n xs
noskip (BSeq [] : xs) = noskip xs
noskip (x : xs) = x : noskip xs
noskip [] = []

skip :: Int -> [BTree] -> [BTree]
skip n (Skip m : xs) = skip (n + m) xs
skip n xs = Skip n : noskip xs

{-
BUG:

>>> data Void deriving (Generic,Model)
>>> (absTypeModel (Proxy::Proxy Void))
TypeModel {typeName = TypeCon (AbsRef (SHAKE128_48 244 22 86 20 212 242)), typeEnv = fromList [(AbsRef (SHAKE128_48 244 22 86 20 212 242),ADT {declName = Name (UnicodeLetter 'V') [UnicodeLetterOrNumberOrLine 'o',UnicodeLetterOrNumberOrLine 'i',UnicodeLetterOrNumberOrLine 'd'], declNumParameters = 0, declCons = Nothing})]}

>>> typeMatcher (absTypeModel (Proxy::Proxy Void))
solve:Unknown reference to TypeCon (AbsRef (SHAKE128_48 244 22 86 20 212 242)) in []

>>> tst ()
(Right (),BSeq [])

>>> tst (Just False)
(Right (),BFork (BSeq []) (Skip 1))

>>> tst (False,True)
(Right (),Skip 2)

>>> tst [False,True,False]
(Right (),BFork (BSeq []) (BSeq [Skip 1,BTyp (TypeApp (TypeCon (AbsRef (SHAKE128_48 184 205 19 24 113 152))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))))]))

>>> newtype NT a = NT a deriving (Show, Eq, Generic,Model)
>>> typeMatcher (absTypeModel (Proxy::Proxy (NT (NT (NT Bool)))))
Skip 1

>>> data ND a = ND a deriving (Show, Eq, Generic,Model)
>>> typeMatcher (absTypeModel (Proxy::Proxy (ND (ND (ND Bool)))))
Skip 1

>>> data T3 = V1 | V2 | V3 deriving (Show, Eq, Generic,Model,Flat)
>>> tst (V1::T3)
(Right (),BFork (BSeq []) (Skip 1))

>>> data T4 = V1 | V2 | V3 | V4 deriving (Show, Eq, Generic,Model,Flat)
>>> tst (V1::T4)
(Right (),Skip 2)

>>> data T5 = V1 | V2 | V3 | V4 | V5 deriving (Show, Eq, Generic,Model,Flat)
>>> tst (V3::T5)
(Right (),BFork (Skip 1) (BFork (BSeq []) (Skip 1)))

>>> data T6 = V1 | V2 | V3 | V4 | V5 | V6 deriving (Show, Eq, Generic,Model,Flat)
>>> tst (V3::T6)
(Right (),BFork (BFork (BSeq []) (Skip 1)) (BFork (BSeq []) (Skip 1)))

>>> import ZM.Type.Bits8
>>> typeMatcher (absTypeModel (Proxy::Proxy Bits8))
Skip 8

>>> import ZM.Type.Bits11
>>> typeMatcher (absTypeModel (Proxy::Proxy Bits11))
Skip 11

>>> typeMatcher (absTypeModel (Proxy::Proxy Word7))
Skip 7

>>> import qualified Data.Text as T
>>> tst (T.pack "abc"::T.Text)
(Right (),BTyp (TypeApp (TypeCon (AbsRef (SHAKE128_48 178 242 140 243 125 18))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 46 139 69 25 174 170))) (TypeCon (AbsRef (SHAKE128_48 177 244 106 73 200 248))))))

>>> tst "abc"
(Right (),BFork (BSeq []) (BSeq [BTyp (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))))),BTyp (TypeApp (TypeCon (AbsRef (SHAKE128_48 184 205 19 24 113 152))) (TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69))))]))

>>> tst 'a'
(Right (),BTyp (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))))))

>>> tst (123::Word8)
(Right (),Skip 8)

>>> tst ((1234)::Word32)
(Right (),BTyp (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))))))

>>> tst (-1234::Int64)
(Right (),BTyp (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))))))

>>> tst (123::Word)
(Right (),BTyp (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))))))

>>> tst (123::Int)
(Right (),BTyp (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))))))

>>> tst (1.1E-33::Float)
(Right (),Skip 32)

>>> tst (1.1E-33::Double)
(Right (),Skip 64)
-}

-- Convert a Pattern to a Matcher
-- patternMatcher :: AbsTypeModel -> Pattern -> PatternMatcher
-- patternMatcher at pat = (typeMatcherMap at, pat)

tst :: forall a. (Flat a, Model a) => a -> (Decoded (), BTree)
tst a = let tm = absTypeModel (Proxy :: Proxy a); t = typeName tm; dm = typeMatcherMap tm in (unflatWith (dWild dm t) (flat a), typeMatcher tm)

-- | Decode/Skip a vale of the indicated abs type
dWild :: M.Map AbsType BTree -> AbsType -> Get ()
dWild tm = matchType
  where
    matchType t = matchTree (solve t tm)
    matchTree (BFork l r) = do
      b <- dBool
      if b
        then matchTree r
        else matchTree l
    matchTree (BSeq ts) = mapM_ matchTree ts
    matchTree (BTyp t) = matchType t
    matchTree (Skip n) = dropBits n

-- | Returns a matcher for the type defined by the given model
typeMatcher :: AbsTypeModel -> BTree
typeMatcher = typeOp typeMatcherMap

-- | A mapping between references to absolute types and the corresponding matcher
type TypeMatcherMap = TypeMap BTree

-- | Returns matchers for all types in the given model
typeMatcherMap :: AbsTypeModel -> TypeMatcherMap
typeMatcherMap = typeOpMap (\tm t ct -> simpler (t, tm) $ conMatcher tm t ct)

conMatcher ::
  TypeMatcherMap ->
  AbsType ->
  ConTree name AbsRef ->
  BTree
conMatcher env t (ConTree l r) = BFork (conMatcher env t l) (conMatcher env t r)
conMatcher env t (Con _ cs) = BSeq $ map BTyp (fieldsTypes cs)

-- conMatcher env _ (Con _ cs) = BSeq $ map (`solve` env) (fieldsTypes cs)

typeOp :: (AbsTypeModel -> TypeMap r) -> AbsTypeModel -> r
typeOp opMap tm = solve (typeName tm) (opMap tm)

-- | A mapping between references to absolute types and the corresponding operation
type TypeMap r = M.Map (Type AbsRef) r

typeOpMap ::
  (TypeMap r -> AbsType -> ConTree Identifier AbsRef -> r) ->
  AbsTypeModel ->
  TypeMap r
typeOpMap op tm =
  let denv = M.mapWithKey (op denv) (typeTree tm) in denv

-- tst n v d = do
--   decoded <- d n
--   let r = xor v decoded
--   -- traceM $ unwords ["compare", show n, "bits of", show v, "with", show decoded]
--   when (r /= 0) $ fail "matcher failed"

-- typeMatcherMap :: AbsTypeModel -> M.Map AbsType BTree
-- typeMatcherMap tm =
--   let env = typeTree tm in M.mapWithKey (\t _ -> simplifyBTree $ typeTr env t) env
--   where
--     typeTr :: MapTypeTree -> AbsType -> BTree
--     typeTr = typeTree_ BFork (\_ _ _ ps -> BSeq (map BTyp ps))

--     typeTree_ f c env t = conTree_ f c t [] (solve t env)

--     conTree_ f c t bs (ConTree l r) = f (conTree_ f c t (0 : bs) l) (conTree_ f c t (1 : bs) r)
--     conTree_ f c t bs (Con cn cs) = c t (convert cn :: String) (reverse bs) (fieldsTypes cs)

-- conTree_ f c e t bs (Con cn cs) = c t (L.toString cn) (reverse bs) (map (typeTree_ f c e) (fieldsTypes cs))

-- decodeAbsTypeModel :: AbsTypeModel -> B.ByteString -> Decoded ()
-- decodeAbsTypeModel = unflatWith . typeMatcher

-- | Simplify BTree substituting, whenever possible, an explicit traverse of the constructor tree
--  with the skip of a certain number of bits
--  For example the constructor tree (False | True) can be matched by: Skip 1 (bit).
-- simplifyBTree :: BTree -> BTree
-- simplifyBTree = brec sym
--   where
--     -- \|Bottom-up tree simplification
--     brec :: (BTree -> Maybe BTree) -> BTree -> BTree
--     brec f n@(BFork l r) =
--       fromMaybe
--         ( let n = BFork (brec f l) (brec f r) in fromMaybe n (f n)
--         )
--         (f n)
--     brec f n = fromMaybe n (f n)

--     -- \|Simplify constructor trees of the same length to a Skip
--     sym :: BTree -> Maybe BTree
--     sym (BFork (BSeq []) (BSeq [])) = Just $ Skip 1
--     sym (BFork (Skip n1) (Skip n2)) | n1 == n2 = Just $ Skip (n1 + 1)
--     sym _ = Nothing
