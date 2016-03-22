{-# LANGUAGE ScopedTypeVariables  #-}
module Data.Typed.Value(typedBytes,typedValue,decodeTypedValue) where

import qualified Data.ByteString.Lazy as L
import Data.Typed.Types
import Data.Typed.Class
import Data.Flat
import Data.Model

instance Flat TypedBytes
instance Flat a => Flat (TypedValue a)

typedBytes :: forall a . (Model a,Flat a) => a -> TypedBytes
typedBytes a = TypedBytes (absType (Proxy :: Proxy a)) (nonEmptyList . L.unpack . flat $ a)

typedValue :: forall a . Model a => a -> TypedValue a
typedValue = TypedValue (absType (Proxy :: Proxy a))

--encodedTyped :: (HasModel a ,Binary a) => a -> Encoded
--encodedTyped = encoded . typedValue

-- |Decode making sure that the type is correct
decodeTypedValue :: (Monad m, Model a, Flat a) => Encoded (TypedValue a) -> m (Either DeserializeFailure a)
decodeTypedValue bs =
  case decoded bs of
    Left e -> return $ Left e
    Right (TypedValue typ' a) -> do
      let typ = absType (proxyOf a)
      if (typ' /= typ)
        then do
          let rt  = show typ -- simpleTypeS typ
              rt' = show typ' -- simpleTypeS typ'
              msg = unwords $ ["Was expecting type:\n",rt,"\n\nBut the data has type:\n",rt']
              -- putStrLn msg
          return . Left $ msg
        else return . Right $ a

-- TODO: rewrite to operate on AbsTypes
-- Generic value (used for dynamic decoding)
-- data Val = Val
--    String -- Constructor name
--    [Bool] -- Bit encoding (for debugging purposes)
--    [Val]  -- Vals to which the constructor is applied, if any
--    deriving Show

-- -- Decode a TypedValue without previous knowledge of its type
-- decodeDynamically :: Encoded a -> Decoded Val
--  decodeDynamically enc =
--     case runPartialGet decode (bytes enc) 0 of
--       Left e -> Left e
--       Right (typ,bs,n) -> case runPartialGet (decoderHType typ) bs n of
--         Left e -> Left e
--         Right r@(v,bs',n') -> Right v -- partBits bs n 

-- decodeDebug :: forall a. (Binary a,HasModel a) => a -> Decoded Val
-- decodeDebug a = 
--      let ty = hType (Proxy :: Proxy a)
--          Encoded bs = encoded a
--      in runGetOrFail (decoderHType ty) bs

-- decoderHType ht = let (t,e) = ctEnv ht in decTyp e t
--    where
--      decTyp e t = decCon e (solve t e)

--      --decCon :: EnvCT -> ConTree (ETypeRef QualName) -> Get Val
--      decCon e = decCT e []

--      decCT e bs (CT l r) = do
--        tag <- getBool
--        decCT e (tag:bs) (if tag then r else l)

--      decCT e bs (CN cn cs) = Val cn (reverse bs) <$> mapM (decTyp e) cs

-- instance Pretty Val where
--    pPrint = pp 0
--      where
--        pp l (Val n bs vs) = (if l>0 && not (null vs) then parens else id) (hsep $ (if null bs then empty else (text (map (\b -> if b then '1' else '0') bs) <> char ':')) <> text n : map (pp (l+1)) vs)

