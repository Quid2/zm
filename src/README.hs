---------- How To Use It For Fun and Profit in Haskell

{-# LANGUAGE DeriveGeneric #-}
module README where

import           Data.Typed

-- Define your data types, derive from Generic.

-- Some numbers in English
data Number = One | Two | Three | Four | Five
        deriving (Show, Generic)

-- Some numbers in Italian
data Numero = Uno | Due | Tre | Quattro | Cinque
            deriving (Show, Generic)

-- A tuple
data Couple a b = Couple a b deriving (Show,Generic)

-- To be able to serialize them we need a Flat instance
-- No big deal, it can be derived automatically.
instance Flat Number
instance Flat Numero
instance (Flat a , Flat b) => Flat (Couple a b)

-- Serialize a value
e1 = encoded $ Couple One Due

-- One has been encoded as '00', Due as '01', the rest is byte-padding:
-- 00010000

-- Now get it back
d1 = decoded e1 :: Decoded (Couple Number Numero)

-- Right (Couple One Due)

-- One more time
d2 :: Decoded (Couple Numero Number)
d2 = decoded . Encoded . bytes $ e1

-- Right (Couple Uno Two)

-- Oops, that's not quite right.
-- We got our types crossed.
-- Number and Numero look similar but they are not quite the same thing.
-- We should have got an error, instead we got an incorrect value back.

-- Let's switch to typed serialisation
-- This is added on top of the basic system and defined into it.

-- We define a data model, that can be calculated automatically
-- We need to derive Model instances for our data types:
instance Model Number
instance Model Numero
instance (Model a , Model b) => Model (Couple a b)

-- Now to every data type has been assigned a globally unique type:
-- ty1 :: Type TypeName
ty1 = absType (Proxy :: Proxy Number)

-- Let's print it in a nicer way, to show that is indeed equivalent to the Number datatype:
pty1 = pp ty1
-- data Number = One | Two | Three | Four | Five

-- Same for the others
ty2 = absType (Proxy :: Proxy Numero)
pty2 = pp ty2

ty3 = absType (Proxy :: Proxy (Couple Numero Number))
pty3 = pp ty3

pp = putStrLn . prettyShow

-- The types are hashed to provide a more compact type signature:
-- ety1 :: IO (Type (Ref (ADT)))
ety1 = absType (Proxy :: Proxy Number)
ety2 = absType (Proxy :: Proxy Numero)
ety3 = absType (Proxy :: Proxy (Couple Numero Number))

-- We combine data with its type.
td1 = typedValue (Couple One Due)

-- It can be encoded as any other value:
-- e2 :: IO Encoded
e2 = encoded $ typedValue (Couple One Due)

-- Now get it back
dt2 = decoded e2 :: Decoded (TypedValue (Couple Number Numero))

-- Right (TypedValue (TypeApp (TypeApp (TypeCon (CRC16 91 93)) (TypeCon (CRC16 79 130))) (TypeCon (CRC16 65 167))) (Couple One Due))

-- To check that the type is correct, use decodeTyped
dt2b = decodeTypedValue e2 :: IO (Decoded (Couple Number Numero))

-- Right (Couple One Due)

-- And now with the wrong type:
dt1 = decodeTypedValue . Encoded . bytes $ e2 :: IO (Decoded (Couple Numero Number))

{-
Was expecting type:
 ((data Couple a b = Couple a b)
 (data Numero = Uno | Due | Tre | Quattro | Cinque)
 (data Number = One | Two | Three | Four | Five))

But the data has type:
 ((data Couple a b = Couple a b)
 (data Number = One | Two | Three | Four | Five)
 (data Numero = Uno | Due | Tre | Quattro | Cinque))
-}
-- Let's check more precisely how this was coded
-- dd1 = decodeDebug (Couple One Tre)

-- That's better, we were not able to deserialise a wrong value and we get a readable error message with a full description of the actual data schema.

{-
-- So far so good but what if we lose the definitions of our data types?
-- Better save them for posterity:
-}

-- sv = saveTypeIn theCloud (Couple One Tre)

-- The type has been saved, with all its dependencies.
-- TypeApp (TypeApp (TypeCon (CRC16 91 93)) (TypeCon (CRC16 79 130))) (TypeCon (CRC16 65 167))

{-
Now that they are safe in the Cloud we can happily burn our code
in the knowledge that when we are presented with a binary of unknown type
we can always recover the full definition of our data.
-}

-- PUT BACK dt = e2 >>= recoverTypeFrom theCloud

{-
 ((data Couple a b = Couple a b)
 (data Number = One | Two | Three | Four | Five)
 (data Numero = Uno | Due | Tre | Quattro | Cinque))
-}

data CoupleB a b = CoupleB Bool a b deriving (Show,Generic)
instance (Model a , Model b) => Model (CoupleB a b)
instance (Flat a , Flat b) => Flat (CoupleB a b)

-- t = ed False >> ed Tre >> ed (Couple (CoupleB True Uno One) Three)
-- ed = pp . decodeDynamically . encoded . typedValue

{-
We can now use it to define a hard-wired decoder.

Or use a dynamic decoder to directly show the value.

The final system will also keep track of the documentation that comes with the types to give you a fully human understandable description of the data.

-}

-- And that's all folks!
