{-# LANGUAGE TemplateHaskell ,DeriveGeneric #-}
module Data.Pattern.Test where

import Data.Pattern.Types
import Data.Pattern.ToHaskell
import Data.Word
import Data.Typed

-- $(defPattern "BellsPatt" ''(Message Content) [p| Message "titto" _ (Bell b)|])

t = writeType (Proxy::Proxy (List (Message Content)))

type Mes = Message Content

data Tuple a b = Tuple a b 

--patternM = Wild

-- k :: (Word8 -> m a) -> (String -> m a) -> m a
-- k = undefined 

-- data M = M0 {_M0v::Word8}
--        | M1 {_M1v::String}

-- match :: Message Content -> Maybe M
-- match m = case m of
--    Message "titto" _ (Bell v) -> Just M0 {_M0v=v}
--    Message _ _ (TextMessage v) -> Just M1 {_M1v=v}
--    _ -> Nothing

data Message c = Message {from::String,code::Int,content::c} deriving Generic
data Content = TextMessage String
               | Join 
               | Bell Word8 deriving Generic

data List a = Cons a | Nil deriving Generic

instance Model a => Model (Message a)
instance Model Content
instance Model a => Model (List a)

-- x = filter [p|\Message _ (subject:_) _ |] 
-- y = patternQ [p|Message "titto" _ (Bell _)|]

-- x = let bySubject = $(filterPatternQ [p|Message _ subj (Bell v)|])
--     in bySubject (prefixPattern ["Haskell","Meeting"]) (valPattern (11::Word8))

x = filterPatternQ [p|Message _ subj (Bell v)|] >>= print

--x = filterPatternQ [p|Message _ subj (Bell v)|]

-- pattern "P" ''Mes [[p|Message "titto" _ (Bell v)|]
--                   ,[p|Message _ _ (TextMessage v)|]
--                   ]

