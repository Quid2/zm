{-# LANGUAGE NoMonomorphismRestriction #-}
module Info where

import           Data.Int
import           ZM
import           Data.Word
import           Test.Data       hiding (Unit)
-- import           Test.Data.Flat  hiding (Unit)
import           Test.Data.Model()
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
    ,typ (Proxy :: Proxy (BLOB UTF8Encoding))
    ,typ (Proxy :: Proxy (BLOB UTF16LEEncoding))
    ]
  where typ = absTypeModel

codes = [TypeApp (TypeApp (TypeApp (TypeCon (AbsRef (SHAKE128_48 62 130 87 37 92 191))) (TypeCon (AbsRef (SHAKE128_48 220 38 233 217 0 71)))) (TypeCon (AbsRef (SHAKE128_48 220 38 233 217 0 71)))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 7 177 176 69 172 60))) (TypeCon (AbsRef (SHAKE128_48 75 189 56 88 123 158))))
        ,TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 212 160 181 74 243 52))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 148 55 180 1 191 163))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 212 160 181 74 243 52))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 212 160 181 74 243 52))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 148 55 180 1 191 163))) (TypeApp (TypeCon (AbsRef (SHAKE128_48 212 160 181 74 243 52))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 144 198 49 59 57 208))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))
        ,TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 184 205 19 24 113 152))) (TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69)))
        ,TypeCon (AbsRef (SHAKE128_48 80 208 24 247 89 58))
        ,TypeCon (AbsRef (SHAKE128_48 177 244 106 73 200 248))
        ,TypeCon (AbsRef (SHAKE128_48 41 94 36 214 47 172))
        ,TypeCon (AbsRef (SHAKE128_48 36 18 121 156 153 241))
        ,TypeCon (AbsRef (SHAKE128_48 80 208 24 247 89 58))
        ,TypeCon (AbsRef (SHAKE128_48 251 148 203 77 78 222))
        ,TypeCon (AbsRef (SHAKE128_48 179 162 100 43 74 132))
        ,TypeCon (AbsRef (SHAKE128_48 61 172 107 212 250 156))
        ,TypeCon (AbsRef (SHAKE128_48 90 31 178 147 33 165))
        ,TypeCon (AbsRef (SHAKE128_48 251 148 203 77 78 222))
        ,TypeCon (AbsRef (SHAKE128_48 16 42 59 185 4 227))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 46 139 69 25 174 170))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 212 160 181 74 243 52))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))
        ,TypeCon (AbsRef (SHAKE128_48 180 209 66 111 91 138))
        ,TypeCon (AbsRef (SHAKE128_48 179 88 47 48 3 203))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 204 16 207 144 173 119))) (TypeCon (AbsRef (SHAKE128_48 121 74 239 110 33 170)))
        ,TypeApp (TypeApp (TypeCon (AbsRef (SHAKE128_48 98 96 228 101 174 116))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))) (TypeCon (AbsRef (SHAKE128_48 121 74 239 110 33 170)))
        ,TypeApp (TypeApp (TypeCon (AbsRef (SHAKE128_48 98 96 228 101 174 116))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))
        ,TypeApp (TypeApp (TypeApp (TypeCon (AbsRef (SHAKE128_48 58 94 167 13 35 164))) (TypeCon (AbsRef (SHAKE128_48 116 153 36 195 148 43)))) (TypeCon (AbsRef (SHAKE128_48 121 74 239 110 33 170)))) (TypeCon (AbsRef (SHAKE128_48 182 24 14 79 250 138)))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 241 57 212 117 31 218))) (TypeCon (AbsRef (SHAKE128_48 15 68 139 232 5 128)))
        ,TypeApp (TypeCon (AbsRef (SHAKE128_48 241 57 212 117 31 218))) (TypeCon (AbsRef (SHAKE128_48 193 183 198 207 63 81)))
        ]
