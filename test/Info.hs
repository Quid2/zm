{-# LANGUAGE NoMonomorphismRestriction #-}
module Info where

import           Data.Int
import           Data.Typed
import           Data.Word
import           Test.Data       hiding (Unit)
import           Test.Data.Flat  hiding (Unit)
import           Test.Data.Model
import qualified Test.Data2      as Data2
import qualified Test.Data3      as Data3
import qualified Data.Sequence         as S

models = [
     typ (Proxy :: Proxy AbsADT)
    ,typ (Proxy :: Proxy Bool)
    ,typ (Proxy :: Proxy (List Bool))
    ,typ (Proxy :: Proxy (Data2.List Bool))
    ,typ (Proxy :: Proxy (Data3.List Bool))
    ,typ (Proxy :: Proxy (List (Data2.List (Data3.List Bool))))
    ,typ (Proxy :: Proxy (Forest2 Bool))
    ,typ (Proxy :: Proxy Char)
    ,typ (Proxy :: Proxy String)
    ,typ (Proxy :: Proxy Word)
    ,typ (Proxy :: Proxy Word8)
    ,typ (Proxy :: Proxy Word16)
    ,typ (Proxy :: Proxy Word32)
    ,typ (Proxy :: Proxy Word64)
    ,typ (Proxy :: Proxy Int)
    ,typ (Proxy :: Proxy Int8)
    ,typ (Proxy :: Proxy Int16)
    ,typ (Proxy :: Proxy Int32)
    ,typ (Proxy :: Proxy Int64)
    ,typ (Proxy :: Proxy Integer)
    ,typ (Proxy :: Proxy (S.Seq Bool))
    ,typ (Proxy :: Proxy (List Bool))
    ,typ (Proxy:: Proxy D2)
    ,typ (Proxy :: Proxy D4)
    ,typ (Proxy :: Proxy (Phantom ()))
    ,typ (Proxy :: Proxy (Either Bool ()))
    ,typ (Proxy :: Proxy (Either Bool Bool))
    ,typ (Proxy :: Proxy (RR Un () N))
    ]
  where typ = absTypeModel

codes = [TypeApp (TypeApp (TypeApp (TypeCon (AbsRef (SHA3_256_6 168 121 233 97 61 136))) (TypeCon (AbsRef (SHA3_256_6 10 197 162 77 118 54)))) (TypeCon (AbsRef (SHA3_256_6 10 197 162 77 118 54)))) (TypeApp (TypeCon (AbsRef (SHA3_256_6 122 206 49 124 168 130))) (TypeCon (AbsRef (SHA3_256_6 123 110 193 184 1 78))))
    ,TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29))
    ,TypeApp (TypeCon (AbsRef (SHA3_256_6 101 57 81 27 13 167))) (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
    ,TypeApp (TypeCon (AbsRef (SHA3_256_6 102 100 14 127 233 210))) (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
    ,TypeApp (TypeCon (AbsRef (SHA3_256_6 101 57 81 27 13 167))) (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
    ,TypeApp (TypeCon (AbsRef (SHA3_256_6 101 57 81 27 13 167))) (TypeApp (TypeCon (AbsRef (SHA3_256_6 102 100 14 127 233 210))) (TypeApp (TypeCon (AbsRef (SHA3_256_6 101 57 81 27 13 167))) (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))))
    ,TypeApp (TypeCon (AbsRef (SHA3_256_6 53 50 106 30 119 81))) (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
    ,TypeCon (AbsRef (SHA3_256_6 7 117 93 14 24 29))
    ,TypeApp (TypeCon (AbsRef (SHA3_256_6 104 188 123 93 245 148))) (TypeCon (AbsRef (SHA3_256_6 7 117 93 14 24 29)))
    ,TypeCon (AbsRef (SHA3_256_6 166 79 5 101 124 185))
    ,TypeCon (AbsRef (SHA3_256_6 9 220 228 4 7 148))
    ,TypeCon (AbsRef (SHA3_256_6 157 196 6 176 134 110))
    ,TypeCon (AbsRef (SHA3_256_6 55 196 92 68 135 146))
    ,TypeCon (AbsRef (SHA3_256_6 166 79 5 101 124 185))
    ,TypeCon (AbsRef (SHA3_256_6 8 245 185 37 4 152))
    ,TypeCon (AbsRef (SHA3_256_6 119 192 116 209 194 141))
    ,TypeCon (AbsRef (SHA3_256_6 177 142 103 96 187 181))
    ,TypeCon (AbsRef (SHA3_256_6 45 3 206 152 7 34))
    ,TypeCon (AbsRef (SHA3_256_6 8 245 185 37 4 152))
    ,TypeCon (AbsRef (SHA3_256_6 168 236 150 6 16 84))
    ,TypeApp (TypeCon (AbsRef (SHA3_256_6 46 45 234 102 228 168))) (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
    ,TypeApp (TypeCon (AbsRef (SHA3_256_6 101 57 81 27 13 167))) (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
    ,TypeCon (AbsRef (SHA3_256_6 217 161 185 121 114 16))
    ,TypeCon (AbsRef (SHA3_256_6 143 138 35 109 231 29))
    ,TypeApp (TypeCon (AbsRef (SHA3_256_6 97 93 162 205 14 216))) (TypeCon (AbsRef (SHA3_256_6 121 90 213 206 19 234)))
    ,TypeApp (TypeApp (TypeCon (AbsRef (SHA3_256_6 173 144 137 143 179 59))) (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))) (TypeCon (AbsRef (SHA3_256_6 121 90 213 206 19 234)))
    ,TypeApp (TypeApp (TypeCon (AbsRef (SHA3_256_6 173 144 137 143 179 59))) (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))) (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
    ,TypeApp (TypeApp (TypeApp (TypeCon (AbsRef (SHA3_256_6 4 57 85 199 173 43))) (TypeCon (AbsRef (SHA3_256_6 162 203 228 37 70 146)))) (TypeCon (AbsRef (SHA3_256_6 121 90 213 206 19 234)))) (TypeCon (AbsRef (SHA3_256_6 33 235 4 113 180 240)))]

