{-# LANGUAGE NoMonomorphismRestriction ,ScopedTypeVariables #-}

import Control.Applicative
import Test.Data
import Test.Data.Flat
import Data.Typed
import Data.Model
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Bifunctor

x = tst (Proxy :: Proxy (List Bool))

tst = absTypeEnv

t =  mapM (prt . prtInstance) [
  tst (Proxy :: Proxy Q.Word7)
  ,tst (Proxy :: Proxy Q.String)
  ,tst (Proxy :: Proxy Q.Char)
  ,tst (Proxy :: Proxy Q.Natural)
  ,tst (Proxy :: Proxy Unit)
  ,tst (Proxy :: Proxy Bool)
  ,tst (Proxy :: Proxy N)
  ,tst (Proxy :: Proxy Un)
  ,tst (Proxy :: Proxy D2)
  ,tst (Proxy :: Proxy D4)
  ,tst (Proxy :: Proxy A0)
  ,tst (Proxy :: Proxy B0)
  ,tst (Proxy :: Proxy (Phantom Unit))
  ,tst (Proxy :: Proxy (List Bool))
  ,tst (Proxy :: Proxy (Either Bool Unit))
  ,tst (Proxy :: Proxy (Either Bool Bool))
  ,tst (Proxy :: Proxy (RR Un Unit N))
  ]

prt o = putStrLn "" >> print o


-- z = absExtTypeOf True >>= (prettyShow <$>) . recoverType
-- type Defs = [(String,ADT)]
-- recoverType :: Type (Ref ADT) -> IO (Maybe (Type TypeName))
-- recoverType (TypeCon ref) = (TypeCon . TypADT <$>) <$> getADT ref
-- recoverType (TypeApp f a) = do
--   mf <- recoverType f
--   ma <- recoverType a
--   return $ TypeApp <$> mf <*> ma

-- readableType :: Type (Ref ADT) -> IO String
-- readableType t = do
--   mt <- recoverType t
--   return $ maybe (unwords ["Unrecognised type:",show t]) prettyShow mt
