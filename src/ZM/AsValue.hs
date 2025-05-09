{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Conversion between ZM values in (simple, without field names) textual form and the corresponding Haskell values

>>> unValue (value 'a') :: Char
'a'

>>> unValue (value (11 :: Word8)) :: Word8
11

Type safe (?):

>>> unValue (value (11 :: Word8)) :: Word32
Wrong Value "V11" []

Actually to an internal form that contains also the absolute type and the binary encoding of the value:

>>> value 'a'
Annotate (TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69)),[]) (ConstrF "Char" (Left [Annotate (TypeCon (AbsRef (SHAKE128_48 36 18 121 156 153 241)),[]) (ConstrF "Word32" (Left [Annotate (TypeCon (AbsRef (SHAKE128_48 249 46 131 57 144 138)),[]) (ConstrF "Word" (Left [Annotate (TypeApp (TypeCon (AbsRef (SHAKE128_48 32 255 172 200 248 201))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))))),[]) (ConstrF "LeastSignificantFirst" (Left [Annotate (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126)))),[False]) (ConstrF "Elem" (Left [Annotate (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))),[]) (ConstrF "MostSignificantFirst" (Left [Annotate (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126)),[True,True,False,False,False,False,True]) (ConstrF "V97" (Left []))]))]))]))]))]))]))

>>> prettyShow $ value 'a'
"(Char (Word32 (Word (LeastSignificantFirst (Elem (MostSignificantFirst V97 ))))))"

>>> prettyShow $ value (11::Word8)
"V11 "

>>> prettyShow $ value (11::Word16)
"(Word16 (Word (LeastSignificantFirst (Elem (MostSignificantFirst V11 )))))"

>>> prettyShow $ value (11::Word)
"(Word64 (Word (LeastSignificantFirst (Elem (MostSignificantFirst V11 )))))"
-}
module ZM.AsValue (
    AsValue (..),
    value,
    unValue,
) where

-- import           Data.Char

-- import qualified ZM.Type.Words   as Z

import qualified Data.Aeson as A
import Data.Annotate
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short as SBS
import Data.Fix
import Data.Int
import qualified Data.Map as M
import Data.Model
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Word
import Data.ZigZag (zagZig)
import Flat (UTF16Text (..), UTF8Text (..))
import Flat.Filler
import Flat.Run
import Numeric.Natural
import ZM.Abs
import qualified ZM.BLOB as Z
import ZM.Parser.Val
import ZM.To.Decoder (decodeAbsTypeModel)
import ZM.Type.Array
import qualified ZM.Type.BLOB as E
import ZM.Type.Bit
import ZM.Type.Bits11
import ZM.Type.Bits23
import ZM.Type.Bits52
import ZM.Type.Bits8
import ZM.Type.Words (Sign (..), Word7 (..))
import ZM.Types

-- #include "MachDeps.h"

{- $setup
>>> :set -XScopedTypeVariables -XDeriveGeneric -XDeriveAnyClass
>>> import ZM
>>> import ZM.Abs
>>> import ZM.Pretty
>>> import Data.Word
>>> import Data.Int
>>> import ZM.Types
>>> import ZM.Parser.Types
>>> import ZM.Type.Words(Word7(..))
>>> import Numeric.Natural
>>> import Test.QuickCheck.Instances.Natural
>>> import Data.List
-}

{-

3
3

"abc"
"abc"

data List a = [] | a "::" (List a)
["[]"]

[1]
["::",1,"[]"]

or special syntax
[1,2,3]
["List",[1,2,3]]

or, more general syntax, a fold:
["Fold","Nil","Cons",[1,2,3,]]

If the expression is typed, we can use this to disambiguate an Array from an ADT.

General typed expression:

[":",3,"Int32"]
[":",]

data Bool = False | True

["False"]
["True"]

data List a = Nil | Cons a (List a)

["Nil"]
["Cons", a, List a]

>>> data URL = URL {domain::String, path::String,port::Int} deriving (Generic,Flat,Model,Show)
>>> error .  A.encode . asJSON $ URL {domain= "www.google.com", path= "/finance",port=8080}
Couldn't match type `ByteString' with `[Char]'
Expected: Value -> [Char]
  Actual: Value -> ByteString
In the first argument of `(.)', namely `encode'
In the second argument of `(.)', namely `encode . asJSON'
In the first argument of `($)', namely `error . encode . asJSON'

data Or a b = a | b
["|", a, b]

>>> asJSON ("abc"::T.Text)
Array [String "BLOB",Array [String "UTF8Encoding"],Array [String "Bytes",Array [String "PreAligned",Array [String "FillerBit",Array [String "FillerBit",Array [String "FillerBit",Array [String "FillerBit",Array [String "FillerBit",Array [String "FillerBit",Array [String "FillerBit",Array [String "FillerEnd"]]]]]]]],Array [String "A3",Array [String "V97"],Array [String "V98"],Array [String "V99"],Array [String "A0"]]]]]

>>> asJSON False
Array [String "False"]

>>> asJSON 'c'
Array [String "Char",Array [String "Word32",Array [String "Word",Array [String "LeastSignificantFirst",Array [String "Elem",Array [String "MostSignificantFirst",Array [String "V99"]]]]]]]

>>> asJSON (11::Word8)
Array [String "V11"]
-}
-- asJSON :: (Model a, Flat a) => a -> A.Value
asJSON :: (Model a, Flat a) => a -> A.Value
asJSON = A.toJSON . annToX Fix . value

data URL = URL {domain :: String, path :: String, port :: Int} deriving (Generic, Model)

class AsValue a where
    -- unVal :: String -> Either [Value] [(String,Value)] -> a

    unVal ::
        -- | constructor name
        String ->
        -- | constructor parameters
        [Value] ->
        -- | equivalente Haskell value
        a

-- | Convert a ZM value in (simple, without field names) textual form in the corresponding Haskell value
unValue :: (AsValue a) => Value -> a
unValue v = unVal (T.unpack $ valName v) (valFields v)

{- Convert an Haskell value in the corresponding ZM value to (simple, without field names) textual form

>>> value (3 ::Int )
Ann {annotation = (TypeCon (AbsRef (SHAKE128_48 251 148 203 77 78 222)),[]), annotated = ConstrF "Int64" (Left [Ann {annotation = (TypeApp (TypeCon (AbsRef (SHAKE128_48 3 34 103 150 237 228))) (TypeCon (AbsRef (SHAKE128_48 80 208 24 247 89 58))),[]), annotated = ConstrF "ZigZag" (Left [Ann {annotation = (TypeCon (AbsRef (SHAKE128_48 80 208 24 247 89 58)),[]), annotated = ConstrF "Word64" (Left [Ann {annotation = (TypeCon (AbsRef (SHAKE128_48 249 46 131 57 144 138)),[]), annotated = ConstrF "Word" (Left [Ann {annotation = (TypeApp (TypeCon (AbsRef (SHAKE128_48 32 255 172 200 248 201))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))))),[]), annotated = ConstrF "LeastSignificantFirst" (Left [Ann {annotation = (TypeApp (TypeCon (AbsRef (SHAKE128_48 191 45 28 134 235 32))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126)))),[False]), annotated = ConstrF "Elem" (Left [Ann {annotation = (TypeApp (TypeCon (AbsRef (SHAKE128_48 116 226 179 184 153 65))) (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126))),[]), annotated = ConstrF "MostSignificantFirst" (Left [Ann {annotation = (TypeCon (AbsRef (SHAKE128_48 244 201 70 51 74 126)),[False,False,False,False,True,True,False]), annotated = ConstrF "V6" (Left [])}])}])}])}])}])}])}])}

>>> value False
Ann {annotation = (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)),[False]), annotated = ConstrF "False" (Left [])}

>>> value (False,True)
Ann {annotation = (TypeApp (TypeApp (TypeCon (AbsRef (SHAKE128_48 165 88 59 243 173 52))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))),[]), annotated = ConstrF "Tuple2" (Left [Ann {annotation = (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)),[False]), annotated = ConstrF "False" (Left [])},Ann {annotation = (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)),[True]), annotated = ConstrF "True" (Left [])}])}

>>> value [True]
Ann {annotation = (TypeApp (TypeCon (AbsRef (SHAKE128_48 184 205 19 24 113 152))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))),[True]), annotated = ConstrF "Cons" (Left [Ann {annotation = (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)),[True]), annotated = ConstrF "True" (Left [])},Ann {annotation = (TypeApp (TypeCon (AbsRef (SHAKE128_48 184 205 19 24 113 152))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))),[False]), annotated = ConstrF "Nil" (Left [])}])}
-}
value :: forall a. (Model a, Flat a) => a -> Value
value a = case decodeAbsTypeModel (absTypeModel (Proxy :: Proxy a)) (flat a) of
    Right v -> v
    Left _ -> error "impossible"

{-
prop> \(w::Word8) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.
-}

{-
prop> \(w::Word8) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Word16) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Word32) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Word64) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Natural) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Int8) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Int16) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Int32) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Int64) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Int) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Word) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

-- >>> map (\w -> (w,unValue (value w) :: Double)) [0 :: Double,20 ..100]

-- >>> let w::Double= 0.2 in unValue (value w) == w

prop> \(w::Integer) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Float) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Double) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(a1::Double,a2::Bool) -> unValue (value (a1,a2)) == (a1,a2)
Add QuickCheck to your cabal dependencies to run this test.

prop> \(a1::Double,a2::Bool,a3::Integer) -> unValue (value (a1,a2,a3)) == (a1,a2,a3)
Add QuickCheck to your cabal dependencies to run this test.

prop> \(a1::Double,a2::Bool,a3::Integer,a4::Word8) -> unValue (value (a1,a2,a3,a4)) == (a1,a2,a3,a4)
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::String) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::[Word16]) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::S.Seq Word16) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

>>> let w=Array [] ::Array Float in unValue (value w) == w
True

>>> let w=Array [1,3..1000::Word16] in unValue (value w) == w
True

prop> \(w::M.Map Int32 Word8) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

>>> unValue (value ())
()

>>> unValue (value E.NoEncoding) == NoEncoding
True

>>> unValue (value E.FlatEncoding) == FlatEncoding
True

>>> unValue (value E.UTF8Encoding) == UTF8Encoding
True

>>> unValue (value E.UTF16LEEncoding) == UTF16LEEncoding
True

prop> \(w::Char) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Maybe Bool) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::Either () Bool) -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

>>> let w=B.empty in unValue (value w) == w
True

>>> let w=B.pack (replicate 300 33) in unValue (value w) == w
True

>>> let w=L.pack (replicate 300 33) in unValue (value w) == w
True

>>> let w=SBS.pack (replicate 300 33) in unValue (value w) == w
True

prop> \(w::T.Text -> unValue (value w) == w
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::T.Text -> unValue (value (UTF8Text w)) == (UTF8Text w)
Add QuickCheck to your cabal dependencies to run this test.

prop> \(w::T.Text -> unValue (value (UTF16Text w)) == (UTF16Text w)
Add QuickCheck to your cabal dependencies to run this test.

-- >>> concat . intersperse "," $ map (\n->concat ["ZM.Type.Bits52.bit",show n]) [0..51]

-- >>> putStr $ prettyShow $ absTypeModel (Proxy :: Proxy Float)
-- >>> unwords $ map (\n->concat["(unValue b",show n,")"]) [0..51]
-- >>> concat . intersperse ","  $ map (\n->concat["b",show n]) [0..51]
-- >>> unwords $ map (\n->concat["bit",show n]) [0..10]
-}

instance (AsValue a) => AsValue (Array a) where
    unVal "A0" [] = Array []
    unVal ('A' : i) vs | let n = read i in n == length vs - 1 && n < 256 = let Array l2 = unValue (last vs) :: Array a in Array (map unValue (init vs) ++ l2)
    unVal v vs = wrongValue v vs

instance (AsValue a) => AsValue (Z.BLOB a) where
    unVal "BLOB" [e, bs] = Z.BLOB (unValue e) (unValue bs)
    unVal v vs = wrongValue v vs

instance AsValue E.NoEncoding where
    unVal "NoEncoding" [] = E.NoEncoding
    unVal v vs = wrongValue v vs

instance AsValue E.FlatEncoding where
    unVal "FlatEncoding" [] = E.FlatEncoding
    unVal v vs = wrongValue v vs

instance AsValue E.UTF8Encoding where
    unVal "UTF8Encoding" [] = E.UTF8Encoding
    unVal v vs = wrongValue v vs

instance AsValue E.UTF16LEEncoding where
    unVal "UTF16LEEncoding" [] = E.UTF16LEEncoding
    unVal v vs = wrongValue v vs

instance AsValue T.Text where
    unVal v vs = T.decodeUtf8 . Z.content $ (unVal v vs :: Z.BLOB UTF8Encoding)

instance AsValue TL.Text where
    unVal v vs = TL.fromStrict (unVal v vs)

instance AsValue UTF8Text where
    unVal v vs = UTF8Text (unVal v vs)

instance AsValue UTF16Text where
    unVal v vs = UTF16Text . T.decodeUtf16LE . Z.content $ (unVal v vs :: Z.BLOB UTF16LEEncoding)

instance AsValue B.ByteString where
    unVal v vs = B.pack (bytes v vs)

instance AsValue L.ByteString where
    unVal v vs = L.pack (bytes v vs)

instance AsValue SBS.ShortByteString where
    unVal v vs = SBS.pack (bytes v vs)

bytes :: String -> [Value] -> [Word8]
bytes v vs = let Bytes (PreAligned _ (Array bs)) = unVal v vs in bs

instance AsValue Bytes where
    unVal "Bytes" [pa] = Bytes (unValue pa)
    unVal v vs = wrongValue v vs

instance (AsValue a) => AsValue (PreAligned a) where
    unVal "PreAligned" [f, a] = PreAligned (unValue f) (unValue a)
    unVal v vs = wrongValue v vs

instance AsValue Filler where
    unVal "FillerBit" [a] = FillerBit (unValue a)
    unVal "FillerEnd" [] = FillerEnd
    unVal v vs = wrongValue v vs

instance (AsValue a, Ord a, AsValue b) => AsValue (M.Map a b) where
    unVal "Map" [a] = M.fromList (unValue a :: [(a, b)])
    unVal v vs = wrongValue v vs

instance (AsValue a) => AsValue (S.Seq a) where
    unVal v vs = S.fromList (unVal v vs :: [a])

instance AsValue () where
    unVal "Unit" [] = ()
    unVal v vs = wrongValue v vs

instance AsValue Char where
    unVal "Char" [a] = toEnum . fromIntegral $ (unValue a :: Word32)
    unVal v vs = wrongValue v vs

instance (AsValue a) => AsValue [a] where
    unVal "Nil" [] = []
    unVal "Cons" [a, l] = unValue a : unValue l
    unVal v vs = wrongValue v vs

instance (AsValue a) => AsValue (Maybe a) where
    unVal "Nothing" [] = Nothing
    unVal "Just" [a] = Just (unValue a)
    unVal v vs = wrongValue v vs

instance (AsValue a, AsValue b) => AsValue (Either a b) where
    unVal "Left" [a] = Left (unValue a)
    unVal "Right" [a] = Right (unValue a)
    unVal v vs = wrongValue v vs

instance AsValue Bool where
    unVal "False" [] = False
    unVal "True" [] = True
    unVal v vs = wrongValue v vs

instance (AsValue a1, AsValue a2) => AsValue (a1, a2) where
    unVal "Tuple2" [a1, a2] = (unValue a1, unValue a2)
    unVal v vs = wrongValue v vs

instance (AsValue a1, AsValue a2, AsValue a3) => AsValue (a1, a2, a3) where
    unVal "Tuple3" [a1, a2, a3] = (unValue a1, unValue a2, unValue a3)
    unVal v vs = wrongValue v vs

instance (AsValue a1, AsValue a2, AsValue a3, AsValue a4) => AsValue (a1, a2, a3, a4) where
    unVal "Tuple4" [a1, a2, a3, a4] =
        (unValue a1, unValue a2, unValue a3, unValue a4)
    unVal v vs = wrongValue v vs

instance AsValue Word7 where
    unVal ('V' : n) [] = case read n :: Word8 of
        w | w < 128 -> Word7 w
        w -> error $ unwords ["unexpected value", show w]
    unVal v vs = wrongValue v vs

-- TODO: remove duplication
-- instance AsValue Z.Word8 where
--   unVal ('V' : n) [] = read n :: Z.Word8
--   unVal v         vs = wrongValue v vs

instance AsValue Word8 where
    unVal ('V' : n) [] = read n :: Word8
    unVal v vs = wrongValue v vs

instance AsValue Word16 where
    unVal "Word16" [a] = fromIntegral (unValue a :: Natural)
    unVal v vs = wrongValue v vs

instance AsValue Word32 where
    unVal "Word32" [a] = fromIntegral (unValue a :: Natural)
    unVal v vs = wrongValue v vs

instance AsValue Word64 where
    unVal "Word64" [a] = fromIntegral (unValue a :: Natural)
    unVal v vs = wrongValue v vs

-- #if WORD_SIZE_IN_BITS == 32
-- #define WORD Word32
-- #define INT  Int32
-- #elif WORD_SIZE_IN_BITS == 64
-- #define WORD Word64
-- #define INT  Int64
-- #else
-- #error expected WORD_SIZE_IN_BITS to be 32 or 64
-- #endif

instance AsValue Word where
    -- unVal a as = fromIntegral (unVal a as :: WORD)
    unVal a as = fromIntegral (unVal a as :: Word)

instance AsValue Int where
    unVal a as = fromIntegral (unVal a as :: Int)

instance AsValue Natural where
    unVal "Word" [a] = let LeastSignificantFirst w = unValue a in asNatural w
    unVal v vs = wrongValue v vs

instance AsValue Int8 where
    unVal "Int8" [a] = let ZigZag w = unValue a in zagZig w
    unVal v vs = wrongValue v vs

instance AsValue Int16 where
    unVal "Int16" [a] = let ZigZag w = unValue a in zagZig w
    unVal v vs = wrongValue v vs

instance AsValue Int32 where
    unVal "Int32" [a] = let ZigZag w = unValue a in zagZig w
    unVal v vs = wrongValue v vs

instance AsValue Int64 where
    unVal "Int64" [a] = let ZigZag w = unValue a in zagZig w
    unVal v vs = wrongValue v vs

instance AsValue Integer where
    unVal "Int" [a] = let ZigZag w = unValue a in zagZig w
    unVal v vs = wrongValue v vs

instance AsValue Float where
    unVal "IEEE_754_binary32" [vsign, mexp, mfrac] =
        let sign = unValue vsign
            MostSignificantFirst (bexp :: Bits8) = unValue mexp
            MostSignificantFirst (bfrac :: Bits23) = unValue mfrac
            expn =
                map
                    (\f -> bool . f $ bexp)
                    [ ZM.Type.Bits8.bit0
                    , ZM.Type.Bits8.bit1
                    , ZM.Type.Bits8.bit2
                    , ZM.Type.Bits8.bit3
                    , ZM.Type.Bits8.bit4
                    , ZM.Type.Bits8.bit5
                    , ZM.Type.Bits8.bit6
                    , ZM.Type.Bits8.bit7
                    ]
            frac =
                map
                    (\f -> bool . f $ bfrac)
                    [ ZM.Type.Bits23.bit0
                    , ZM.Type.Bits23.bit1
                    , ZM.Type.Bits23.bit2
                    , ZM.Type.Bits23.bit3
                    , ZM.Type.Bits23.bit4
                    , ZM.Type.Bits23.bit5
                    , ZM.Type.Bits23.bit6
                    , ZM.Type.Bits23.bit7
                    , ZM.Type.Bits23.bit8
                    , ZM.Type.Bits23.bit9
                    , ZM.Type.Bits23.bit10
                    , ZM.Type.Bits23.bit11
                    , ZM.Type.Bits23.bit12
                    , ZM.Type.Bits23.bit13
                    , ZM.Type.Bits23.bit14
                    , ZM.Type.Bits23.bit15
                    , ZM.Type.Bits23.bit16
                    , ZM.Type.Bits23.bit17
                    , ZM.Type.Bits23.bit18
                    , ZM.Type.Bits23.bit19
                    , ZM.Type.Bits23.bit20
                    , ZM.Type.Bits23.bit21
                    , ZM.Type.Bits23.bit22
                    ]
         in ieee sign expn frac 127
    unVal v vs = wrongValue v vs

instance AsValue Double where
    unVal "IEEE_754_binary64" [vsign, mexp, mfrac] =
        let sign = unValue vsign
            MostSignificantFirst (bexp :: Bits11) = unValue mexp
            MostSignificantFirst (bfrac :: Bits52) = unValue mfrac
            expn =
                map
                    (\f -> bool . f $ bexp)
                    [ ZM.Type.Bits11.bit0
                    , ZM.Type.Bits11.bit1
                    , ZM.Type.Bits11.bit2
                    , ZM.Type.Bits11.bit3
                    , ZM.Type.Bits11.bit4
                    , ZM.Type.Bits11.bit5
                    , ZM.Type.Bits11.bit6
                    , ZM.Type.Bits11.bit7
                    , ZM.Type.Bits11.bit8
                    , ZM.Type.Bits11.bit9
                    , ZM.Type.Bits11.bit10
                    ]
            frac =
                map
                    (\f -> bool . f $ bfrac)
                    [ ZM.Type.Bits52.bit0
                    , ZM.Type.Bits52.bit1
                    , ZM.Type.Bits52.bit2
                    , ZM.Type.Bits52.bit3
                    , ZM.Type.Bits52.bit4
                    , ZM.Type.Bits52.bit5
                    , ZM.Type.Bits52.bit6
                    , ZM.Type.Bits52.bit7
                    , ZM.Type.Bits52.bit8
                    , ZM.Type.Bits52.bit9
                    , ZM.Type.Bits52.bit10
                    , ZM.Type.Bits52.bit11
                    , ZM.Type.Bits52.bit12
                    , ZM.Type.Bits52.bit13
                    , ZM.Type.Bits52.bit14
                    , ZM.Type.Bits52.bit15
                    , ZM.Type.Bits52.bit16
                    , ZM.Type.Bits52.bit17
                    , ZM.Type.Bits52.bit18
                    , ZM.Type.Bits52.bit19
                    , ZM.Type.Bits52.bit20
                    , ZM.Type.Bits52.bit21
                    , ZM.Type.Bits52.bit22
                    , ZM.Type.Bits52.bit23
                    , ZM.Type.Bits52.bit24
                    , ZM.Type.Bits52.bit25
                    , ZM.Type.Bits52.bit26
                    , ZM.Type.Bits52.bit27
                    , ZM.Type.Bits52.bit28
                    , ZM.Type.Bits52.bit29
                    , ZM.Type.Bits52.bit30
                    , ZM.Type.Bits52.bit31
                    , ZM.Type.Bits52.bit32
                    , ZM.Type.Bits52.bit33
                    , ZM.Type.Bits52.bit34
                    , ZM.Type.Bits52.bit35
                    , ZM.Type.Bits52.bit36
                    , ZM.Type.Bits52.bit37
                    , ZM.Type.Bits52.bit38
                    , ZM.Type.Bits52.bit39
                    , ZM.Type.Bits52.bit40
                    , ZM.Type.Bits52.bit41
                    , ZM.Type.Bits52.bit42
                    , ZM.Type.Bits52.bit43
                    , ZM.Type.Bits52.bit44
                    , ZM.Type.Bits52.bit45
                    , ZM.Type.Bits52.bit46
                    , ZM.Type.Bits52.bit47
                    , ZM.Type.Bits52.bit48
                    , ZM.Type.Bits52.bit49
                    , ZM.Type.Bits52.bit50
                    , ZM.Type.Bits52.bit51
                    ]
         in ieee sign expn frac 1023
    unVal v vs = wrongValue v vs

bool :: Bit -> Bool
bool V0 = False
bool V1 = True

instance AsValue Sign where
    unVal "Positive" [] = Positive
    unVal "Negative" [] = Negative
    unVal v vs = wrongValue v vs

instance AsValue Bit where
    unVal "V0" [] = V0
    unVal "V1" [] = V1
    unVal v vs = wrongValue v vs

instance AsValue Bits8 where
    unVal "Bits8" [b0, b1, b2, b3, b4, b5, b6, b7] =
        Bits8
            (unValue b0)
            (unValue b1)
            (unValue b2)
            (unValue b3)
            (unValue b4)
            (unValue b5)
            (unValue b6)
            (unValue b7)
    unVal v vs = wrongValue v vs

instance AsValue Bits11 where
    unVal "Bits11" [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10] =
        Bits11
            (unValue b0)
            (unValue b1)
            (unValue b2)
            (unValue b3)
            (unValue b4)
            (unValue b5)
            (unValue b6)
            (unValue b7)
            (unValue b8)
            (unValue b9)
            (unValue b10)
    unVal v vs = wrongValue v vs

instance AsValue Bits23 where
    unVal "Bits23" [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22] =
        Bits23
            (unValue b0)
            (unValue b1)
            (unValue b2)
            (unValue b3)
            (unValue b4)
            (unValue b5)
            (unValue b6)
            (unValue b7)
            (unValue b8)
            (unValue b9)
            (unValue b10)
            (unValue b11)
            (unValue b12)
            (unValue b13)
            (unValue b14)
            (unValue b15)
            (unValue b16)
            (unValue b17)
            (unValue b18)
            (unValue b19)
            (unValue b20)
            (unValue b21)
            (unValue b22)
    unVal v vs = wrongValue v vs

instance AsValue Bits52 where
    unVal "Bits52" [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31, b32, b33, b34, b35, b36, b37, b38, b39, b40, b41, b42, b43, b44, b45, b46, b47, b48, b49, b50, b51] =
        Bits52
            (unValue b0)
            (unValue b1)
            (unValue b2)
            (unValue b3)
            (unValue b4)
            (unValue b5)
            (unValue b6)
            (unValue b7)
            (unValue b8)
            (unValue b9)
            (unValue b10)
            (unValue b11)
            (unValue b12)
            (unValue b13)
            (unValue b14)
            (unValue b15)
            (unValue b16)
            (unValue b17)
            (unValue b18)
            (unValue b19)
            (unValue b20)
            (unValue b21)
            (unValue b22)
            (unValue b23)
            (unValue b24)
            (unValue b25)
            (unValue b26)
            (unValue b27)
            (unValue b28)
            (unValue b29)
            (unValue b30)
            (unValue b31)
            (unValue b32)
            (unValue b33)
            (unValue b34)
            (unValue b35)
            (unValue b36)
            (unValue b37)
            (unValue b38)
            (unValue b39)
            (unValue b40)
            (unValue b41)
            (unValue b42)
            (unValue b43)
            (unValue b44)
            (unValue b45)
            (unValue b46)
            (unValue b47)
            (unValue b48)
            (unValue b49)
            (unValue b50)
            (unValue b51)
    unVal v vs = wrongValue v vs

instance (AsValue a) => AsValue (ZigZag a) where
    unVal "ZigZag" [a] = ZigZag (unValue a)
    unVal v vs = wrongValue v vs

instance (AsValue a) => AsValue (LeastSignificantFirst a) where
    unVal "LeastSignificantFirst" [a] = LeastSignificantFirst (unValue a)
    unVal v vs = wrongValue v vs

instance (AsValue a) => AsValue (MostSignificantFirst a) where
    unVal "MostSignificantFirst" [a] = MostSignificantFirst (unValue a)
    unVal v vs = wrongValue v vs

asNatural :: NonEmptyList (MostSignificantFirst Word7) -> Natural
asNatural = addLSFList . map unWord7 . asList

{- |
>>> addLSFList []
0

>>> addLSFList [3]
3

>>> addLSFList [127,1]
255
-}
addLSFList :: [Word8] -> Natural
addLSFList =
    fst
        . foldl
            (\(t, e) n -> (t + fromIntegral n * 2 ^ e, e + 7))
            (0 :: Natural, 0 :: Natural)

unWord7 :: MostSignificantFirst Word7 -> Word8
unWord7 (MostSignificantFirst (Word7 n)) = n

asList :: NonEmptyList a -> [a]
asList (Elem a) = [a]
asList (Cons a l) = a : asList l

instance (AsValue a) => AsValue (NonEmptyList a) where
    unVal "Elem" [a] = Elem (unValue a)
    unVal "Cons" [a, l] = Cons (unValue a) (unValue l)
    unVal v vs = wrongValue v vs

ieee :: (Fractional b) => Sign -> [Bool] -> [Bool] -> Int -> b
ieee sign exps fracs expOff =
    let signV Positive = 1
        signV Negative = -1
        expV = bitsVal exps
        fracBits = True : fracs
        fracV = bitsVal fracBits
        val = fromIntegral fracV * (2 ^^ (expV - expOff - length fracBits + 1))
     in signV sign * val

{- |
MSF bitsVal

>>> bitsVal [True,True,False,False,False]
24
-}
bitsVal :: [Bool] -> Int
bitsVal =
    fst
        . foldl
            (\(t, e) n -> (t + fromIntegral (fromEnum n) * 2 ^ e, e + 1))
            (0 :: Int, 0 :: Int)
        . reverse

wrongValue :: (Show a1, Show a2) => a1 -> a2 -> a3
wrongValue v vs = error $ unwords ["Wrong Value", show v, show vs]
