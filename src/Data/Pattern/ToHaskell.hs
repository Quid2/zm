{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections           ,ViewPatterns ,OverloadedStrings #-}
module Data.Pattern.ToHaskell where

import           Control.Monad
import qualified Control.Monad.Reader    as R
import           Control.Monad.Trans.RWS
import           Data.Bifunctor
import qualified Data.ByteString         as B
import           Data.Digest.Shake128
import           Data.Either
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Pattern.Types      as P
import qualified Data.Text               as T
import qualified Data.Text.IO               as T
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Typed              hiding (mdlName)
import           Data.Typed.Transform
import           Data.Word
import           System.Directory
import           System.FilePath
-- data Module = Module {mdlName::String,mdlImports::[String]} deriving Show

 -- instance Pretty Module where
 --   pPrint m = text "module" <+> text (mdlName m) <+> text "where"
 --      <+> vcat (map (\s -> text "import" <+> text s) (mdlImports m))

 -- return $ Module (concat [declName adt,"_",prettyShow ref]) []

 {-
 Message "titto" _ (Bell _)|] 

 -- compare in binary, we need the types of the wildcards
 -- 
 match binaryVal = 
   chkBits [MessageCode..] chkBits [.."titto"] (skip :: Proxy Int) 

 skip :: const () <$> get 

 -- compare in code, need to be able to print out values corresponing to Vals (so know their types and figure out if they are infinite values)
 match v = case v of
   Message "titto" _ (Bell _) -> True
-}

writePattern dir p pat = do
  body <- pattern dir p pat
  let name = T.cons 'P' . T.pack . concatMap hex . B.unpack . shake128 6 . encodeUtf8 $ body
  let header = T.unlines [T.concat ["module QQ.Pattern.",name," (match) where"]
                       ,"import Prelude()"
                       ,"import Quid2.QQ.Pattern"]
  let mdl = T.append header body
  T.putStrLn mdl
  write (concat [dir,"/QQ/Pattern/",T.unpack name,".hs"]) mdl
  return $ T.append "QQ.Pattern." name

-- |Generate code for Pattern match
pattern :: Model a => FilePath -> Proxy a -> P.Pattern P.WildCard -> IO T.Text
pattern dir p pat = do
  let AbsoluteType e t = absoluteType p
  print t
  -- Create all types, the pattern depends on
  mdls <- writeModules dir e

  -- Convert to a matcher
  let convert (P.Con n ps) t =
        let adt = solvedADT e t
        in case consIn n adt of
          Nothing -> [Left $ unwords ["Constructor '",T.unpack n,"' not present in",show adt]]
          Just (bs,ts) -> Right (MatchBits bs) : concatMap (uncurry convert) (zip ps ts)
      convert (P.Var P.WildCard) t = [Right $ MatchType $ solveF mdls t]
      convert (P.Val bs) _ = [Right $ MatchBits bs]

  let r = convert pat t
  if null (lefts r)
     then return . T.pack . srcMatch . rights $ r -- optMatch . 
     else error (unlines $ lefts r)

-- |A pattern matcher
data Match r = MatchBits [Bool]    -- Match a flattened value
             | MatchType (Type r)  -- Match a wildcard of given type
             deriving (Functor,Foldable,Traversable)

-- |Optimise matcher
optMatch (MatchBits bs:MatchBits bs':t) = optMatch $ MatchBits (bs ++ bs'):t 
optMatch (x:xs) = x : optMatch xs
optMatch [] = []

-- |Convert matcher to source code
srcMatch :: [Match DT] -> String
srcMatch ms = unlines $ concat [
  map (("import qualified " ++) . show . dtImport) $ concatMap toList ms
  ,["match :: ByteString -> Bool"]
  ,["match = run [" ++ (concat . intersperse "," . filter (not.null) . concatMap src $ ms) ++ "]"]
  ]
  where
    src (MatchBits [])                  = [[]]
    src (MatchBits bs) | length bs <= 8   = ss ["match8",show $ length bs,show $ asNum bs]
    src (MatchBits bs) | length bs <= 16  = ss ["match16",show $ length bs,show $ asNum bs]
    src (MatchBits bs) | length bs <= 32  = ss ["match32",show $ length bs,show $ asNum bs]
    src (MatchBits bs) | length bs <= 64  = ss ["match64",show $ length bs,show $ asNum bs]
    src (MatchBits bs) = let (bs',bs'') = splitAt 64 bs
                         in src (MatchBits bs') ++ src (MatchBits bs'')
    src (MatchType t) = ss ["skip (decode :: Get (",prettyShow (dtRef <$> t),"))"]
    ss = (:[]) . unwords
    --as8 bs = foldl (\v b -> b) (reverse bs) 0  -- sum . map (\(p,b) -> if b then p else 0) . zip (map (2^) [0..n]) $ bs
    asNum bs = sum . map (\(n,b) -> if b then 2^n else 0) . zip [0..] . reverse $ bs

-- |Monad for generating modules
type M = RWST ADTEnv () MdlState Identity

-- |The generated modules
type MdlState = M.Map AbsRef DT

-- |A module
data Mdl = Mdl {mdlDir::T.Text
               ,mdlName::T.Text
               ,mdlSrc::T.Text
               } deriving (Eq,Show)

mdlFileName r = T.concat [mdlDir r,"/",mdlName r,".hs"]

mdlImport r = txt (mdlDir r) <> char '.' <> txt (mdlName r)

-- |A data type, defined in a module.
data DT = DT {dtMdl::Mdl
             ,dtName::T.Text
             } deriving (Show,Eq)

-- |reference to a module, e.g. QQ.Bool_H2637s7sd8
dtImport = mdlImport . dtMdl

-- |reference to a data type, e.g. QQ.Bool_H2637s7sd8.Bool
dtRef dt = dtImport dt <> char '.' <> txt (dtName dt)

writeType srcDir = writeModules srcDir . canonicalEnv . absoluteType

writeModules :: FilePath -> ADTEnv -> IO MdlState
writeModules dir e =
  let st = modules e
  in do
    mapM_ out . nub . map dtMdl . M.elems $ st
    return st
       where
      out mdl = write (dir </> T.unpack (mdlFileName mdl)) (mdlSrc mdl)

modules :: ADTEnv -> MdlState
modules e = fst $ execRWS (mapM moduleE $ M.keys e) e (M.empty::MdlState)

moduleE :: AbsRef -> M DT
moduleE ref = do
  mMdl <- gets (M.lookup ref)
  maybe (moduleS ref) return mMdl

-- |Generate a module for an ADT
moduleS :: AbsRef -> M DT
moduleS ref = do
      adt <- asks (fromJust . M.lookup ref) -- unused?
      -- let adt = adt0 {declName = LocalName $ declName adt0}      
      let nameS = declName adt
      let name = moduleNameD_ (nameS,ref)
      adt' <- (\adt -> adt {declName = LocalName $ nameS}) <$> (traverse (conv nameS) adt)
      let src = vcat [txt "{-# LANGUAGE DeriveGeneric ,DeriveDataTypeable ,EmptyDataDecls #-}"
                     ,txt "module" <+> dtImport dt <+> txt "where"
                     ,txt "import Prelude()"
                     ,txt "import Quid2.QQ.DataType"
                     ,vcat . map (\r -> txt "import qualified" <+> dtImport r) $ extRefs adt'
                     ,(\dt -> pPrint dt <+> txt "deriving (Eq,Ord,Show,Typeable,Generic)") adt'
                     ,txt . instanceADT "Flat" $ adt'
                     ]
          dt = DT (Mdl "QQ" (tshow name) (tshow src)) (declName adt)
      modify (M.insert ref dt)
      return dt
    where
    conv _ (Ext r) = ExtP <$> moduleE r
    conv _ (Var v) = return $ VarP v
    conv n Rec  = return $ RecP n

    extRefs = nub . mapMaybe fromExt . toList

    fromExt (ExtP d) = Just d
    fromExt _ = Nothing

    --moduleNameD adt = hcat . punctuate (text "_") . map moduleNameD_ <$> (mapM refByName . sort . map declName . toList $ adt)
    -- BAD
    -- refByName n = asks ((n,) . fst . fromJust . find ((n==) . adtName . snd) . M.toList)
    moduleNameD_ (name,ref) = txt name <> char '_' <> pPrint ref

data RefP = VarP Word8
          | RecP T.Text
          | ExtP DT
          deriving (Show)

instance Pretty RefP where
  pPrint (VarP v) = varP v
  pPrint (RecP s) = txt s
  pPrint (ExtP dt) = mdlImport (dtMdl dt) <> char '.' <> txt (dtName dt)

instanceADT cls adt = instanceCtx cls (let LocalName n = declName adt in n) (fromIntegral . declNumParameters $ adt)

instanceCtx cls dt 0 = T.unwords ["instance",cls,dt]
instanceCtx cls dt n =
   let vs = map T.singleton $ take n ['a'..]
   in T.unwords ["instance (",T.intercalate "," . map (\v -> T.unwords [cls,v]) $ vs,") =>",cls,"(",dt,T.unwords vs,")"]

instanceS :: (AbsType,ADTEnv) -> T.Text
instanceS (t,e) = R.runReader (instanceE t) e

instanceE (typeN -> TypeN t []) = do
  name <- R.asks (name t)
  return $ T.unwords ["instance AbsoluteType",name,"where absoluteType _ =","TypeCon (",T.pack $ show t,")"]

instanceE (typeN -> TypeN t ps) = do
  name <- R.asks (name t)
  let
      n = length ps
      vs = map T.singleton $ take n ['a'..]
      ctx = T.intercalate "," . map (T.append "AbsoluteType ") $ vs
      body = strApp (T.concat["TypeCon ",par (T.pack $ show t)]) $ map (\v -> T.concat ["absoluteType (Proxy::Proxy ",v,")"]) vs
   in return $ T.unwords ["instance (",ctx,") => AbsoluteType (",name,T.unwords vs,") where absoluteType _ =",body]

strApp = foldl (\h f -> T.unwords ["TypeApp",par f,par h])

par s = T.concat ["(",s,")"]

-- Env ops
name :: AbsRef -> ADTEnv -> T.Text
name t = declName . solve t

  -- name :: AbsRef -> AbsEnv -> String
 --name t = snd . solver t

-- solver :: AbsRef -> AbsEnv -> (AbsRef,String)
-- solver t = second adtName . refByExt t

-- refByExt :: AbsRef -> AbsEnv -> (AbsRef,AbsADT)
-- refByExt r = fromJust . find ((r==) . fst) . M.elems

-- mutualAdtName = declName . relADT

write f content = do
  createDirectoryIfMissing True (takeDirectory f)
  exists <- doesFileExist f
  -- unless exists $
  T.writeFile f content

tshow = T.pack . show 
