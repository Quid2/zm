
[![Build Status](https://travis-ci.org/Quid2/zm.svg?branch=master)](https://travis-ci.org/Quid2/zm) [![Hackage version](https://img.shields.io/hackage/v/zm.svg)](http://hackage.haskell.org/package/zm)
[![Stackage Nightly](http://stackage.org/package/zm/badge/nightly)](http://stackage.org/nightly/package/zm)
[![Stackage LTS](http://stackage.org/package/zm/badge/lts)](http://stackage.org/lts/package/zm)

Haskell implementation of 正名 (read as: [Zhèng Míng](https://translate.google.com/#auto/en/%E6%AD%A3%E5%90%8D)) a minimalistic, expressive and language independent data modelling language ([specs](http://quid2.org/docs/ZhengMing.pdf)).

### How To Use It For Fun and Profit

With `zm` you can derive and manipulate canonical and language-independent definitions and unique identifiers of (a subset) of Haskell data types.

This can be used, for example:

* in combination with a serialisation library to provide type-safe deserialisation
* for data exchange across different programming languages and software systems
* for long term data preservation

#### Canonical Models of Haskell Data Types

For a data type to have a canonical representation, it has to implement the `Model` type class.

Instances for a few common data types (Bool, Maybe, Tuples, Lists, Ints, Words, String, Text ..) are already defined and there is `Generics` based support to automatically derive additional instances.

Let's see some code, we need a couple of GHC extensions:

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, NoMonomorphismRestriction #-}
```

Import the library:

```haskell
import ZM
```

We use `absTypeModel` to get the canonical type of `Maybe Bool` and `pPrint` to print it nicely:

```haskell
pPrint $ absTypeModel (Proxy :: Proxy (Maybe Bool))
-> Type:
-> 
-> Kda6836778fd4 K306f1981b41c:
-> Maybe Bool
-> 
-> Environment:
-> 
-> Bool.K306f1981b41c ≡   False
->                      | True;
-> 
-> Maybe.Kda6836778fd4 a ≡   Nothing
->                         | Just a;
```


We can see how the data types `Maybe` and `Bool` have been assigned unique canonical identifiers and how the type `Maybe Bool` is accordingly represented.

Contrary to Haskell, `ZhengMing` has no 'magic' built-in types so even something as basic as a `Char` or a `Word` have to be defined explicitly.

For example, a `Word7` (an unsigned integer of 7 bits length) is defined as an explicit enumeration of all the 128 different values that can fit in 7 bits:

```haskell
pPrint $ absTypeModel (Proxy :: Proxy Word7)
-> Type:
-> 
-> Kf4c946334a7e:
-> Word7
-> 
-> Environment:
-> 
-> Word7.Kf4c946334a7e ≡   V0
->                       | V1
->                       | V2
->                       | V3
->                       | V4
->                       | V5
->                       | V6
->                       | V7
->                       | V8
->                       | V9
->                       | V10
->                       | V11
->                       | V12
->                       | V13
->                       | V14
->                       | V15
->                       | V16
->                       | V17
->                       | V18
->                       | V19
->                       | V20
->                       | V21
->                       | V22
->                       | V23
->                       | V24
->                       | V25
->                       | V26
->                       | V27
->                       | V28
->                       | V29
->                       | V30
->                       | V31
->                       | V32
->                       | V33
->                       | V34
->                       | V35
->                       | V36
->                       | V37
->                       | V38
->                       | V39
->                       | V40
->                       | V41
->                       | V42
->                       | V43
->                       | V44
->                       | V45
->                       | V46
->                       | V47
->                       | V48
->                       | V49
->                       | V50
->                       | V51
->                       | V52
->                       | V53
->                       | V54
->                       | V55
->                       | V56
->                       | V57
->                       | V58
->                       | V59
->                       | V60
->                       | V61
->                       | V62
->                       | V63
->                       | V64
->                       | V65
->                       | V66
->                       | V67
->                       | V68
->                       | V69
->                       | V70
->                       | V71
->                       | V72
->                       | V73
->                       | V74
->                       | V75
->                       | V76
->                       | V77
->                       | V78
->                       | V79
->                       | V80
->                       | V81
->                       | V82
->                       | V83
->                       | V84
->                       | V85
->                       | V86
->                       | V87
->                       | V88
->                       | V89
->                       | V90
->                       | V91
->                       | V92
->                       | V93
->                       | V94
->                       | V95
->                       | V96
->                       | V97
->                       | V98
->                       | V99
->                       | V100
->                       | V101
->                       | V102
->                       | V103
->                       | V104
->                       | V105
->                       | V106
->                       | V107
->                       | V108
->                       | V109
->                       | V110
->                       | V111
->                       | V112
->                       | V113
->                       | V114
->                       | V115
->                       | V116
->                       | V117
->                       | V118
->                       | V119
->                       | V120
->                       | V121
->                       | V122
->                       | V123
->                       | V124
->                       | V125
->                       | V126
->                       | V127;
```


A `Word32` can be defined as a `NonEmptyList` list of `Word7`s (a definition equivalent to the [Base 128 Varints encoding](https://developers.google.com/protocol-buffers/docs/encoding#varints)).

```haskell
pPrint $ absTypeModel (Proxy :: Proxy Word32)
-> Type:
-> 
-> K2412799c99f1:
-> Word32
-> 
-> Environment:
-> 
-> LeastSignificantFirst.K20ffacc8f8c9 a ≡   LeastSignificantFirst a;
-> 
-> MostSignificantFirst.K74e2b3b89941 a ≡   MostSignificantFirst a;
-> 
-> NonEmptyList.Kbf2d1c86eb20 a ≡   Elem a
->                                | Cons a (NonEmptyList.Kbf2d1c86eb20 a);
-> 
-> Word.Kf92e8339908a ≡   Word (LeastSignificantFirst.K20ffacc8f8c9 (NonEmptyList.Kbf2d1c86eb20 (MostSignificantFirst.K74e2b3b89941 Word7.Kf4c946334a7e)));
-> 
-> Word32.K2412799c99f1 ≡   Word32 Word.Kf92e8339908a;
-> 
-> Word7.Kf4c946334a7e ≡   V0
->                       | V1
->                       | V2
->                       | V3
->                       | V4
->                       | V5
->                       | V6
->                       | V7
->                       | V8
->                       | V9
->                       | V10
->                       | V11
->                       | V12
->                       | V13
->                       | V14
->                       | V15
->                       | V16
->                       | V17
->                       | V18
->                       | V19
->                       | V20
->                       | V21
->                       | V22
->                       | V23
->                       | V24
->                       | V25
->                       | V26
->                       | V27
->                       | V28
->                       | V29
->                       | V30
->                       | V31
->                       | V32
->                       | V33
->                       | V34
->                       | V35
->                       | V36
->                       | V37
->                       | V38
->                       | V39
->                       | V40
->                       | V41
->                       | V42
->                       | V43
->                       | V44
->                       | V45
->                       | V46
->                       | V47
->                       | V48
->                       | V49
->                       | V50
->                       | V51
->                       | V52
->                       | V53
->                       | V54
->                       | V55
->                       | V56
->                       | V57
->                       | V58
->                       | V59
->                       | V60
->                       | V61
->                       | V62
->                       | V63
->                       | V64
->                       | V65
->                       | V66
->                       | V67
->                       | V68
->                       | V69
->                       | V70
->                       | V71
->                       | V72
->                       | V73
->                       | V74
->                       | V75
->                       | V76
->                       | V77
->                       | V78
->                       | V79
->                       | V80
->                       | V81
->                       | V82
->                       | V83
->                       | V84
->                       | V85
->                       | V86
->                       | V87
->                       | V88
->                       | V89
->                       | V90
->                       | V91
->                       | V92
->                       | V93
->                       | V94
->                       | V95
->                       | V96
->                       | V97
->                       | V98
->                       | V99
->                       | V100
->                       | V101
->                       | V102
->                       | V103
->                       | V104
->                       | V105
->                       | V106
->                       | V107
->                       | V108
->                       | V109
->                       | V110
->                       | V111
->                       | V112
->                       | V113
->                       | V114
->                       | V115
->                       | V116
->                       | V117
->                       | V118
->                       | V119
->                       | V120
->                       | V121
->                       | V122
->                       | V123
->                       | V124
->                       | V125
->                       | V126
->                       | V127;
```


And finally a `Char` can be defined as a tagged `Word32`:

```haskell
pPrint $ absTypeModel (Proxy :: Proxy Char)
-> Type:
-> 
-> K066db52af145:
-> Char
-> 
-> Environment:
-> 
-> Char.K066db52af145 ≡   Char Word32.K2412799c99f1;
-> 
-> LeastSignificantFirst.K20ffacc8f8c9 a ≡   LeastSignificantFirst a;
-> 
-> MostSignificantFirst.K74e2b3b89941 a ≡   MostSignificantFirst a;
-> 
-> NonEmptyList.Kbf2d1c86eb20 a ≡   Elem a
->                                | Cons a (NonEmptyList.Kbf2d1c86eb20 a);
-> 
-> Word.Kf92e8339908a ≡   Word (LeastSignificantFirst.K20ffacc8f8c9 (NonEmptyList.Kbf2d1c86eb20 (MostSignificantFirst.K74e2b3b89941 Word7.Kf4c946334a7e)));
-> 
-> Word32.K2412799c99f1 ≡   Word32 Word.Kf92e8339908a;
-> 
-> Word7.Kf4c946334a7e ≡   V0
->                       | V1
->                       | V2
->                       | V3
->                       | V4
->                       | V5
->                       | V6
->                       | V7
->                       | V8
->                       | V9
->                       | V10
->                       | V11
->                       | V12
->                       | V13
->                       | V14
->                       | V15
->                       | V16
->                       | V17
->                       | V18
->                       | V19
->                       | V20
->                       | V21
->                       | V22
->                       | V23
->                       | V24
->                       | V25
->                       | V26
->                       | V27
->                       | V28
->                       | V29
->                       | V30
->                       | V31
->                       | V32
->                       | V33
->                       | V34
->                       | V35
->                       | V36
->                       | V37
->                       | V38
->                       | V39
->                       | V40
->                       | V41
->                       | V42
->                       | V43
->                       | V44
->                       | V45
->                       | V46
->                       | V47
->                       | V48
->                       | V49
->                       | V50
->                       | V51
->                       | V52
->                       | V53
->                       | V54
->                       | V55
->                       | V56
->                       | V57
->                       | V58
->                       | V59
->                       | V60
->                       | V61
->                       | V62
->                       | V63
->                       | V64
->                       | V65
->                       | V66
->                       | V67
->                       | V68
->                       | V69
->                       | V70
->                       | V71
->                       | V72
->                       | V73
->                       | V74
->                       | V75
->                       | V76
->                       | V77
->                       | V78
->                       | V79
->                       | V80
->                       | V81
->                       | V82
->                       | V83
->                       | V84
->                       | V85
->                       | V86
->                       | V87
->                       | V88
->                       | V89
->                       | V90
->                       | V91
->                       | V92
->                       | V93
->                       | V94
->                       | V95
->                       | V96
->                       | V97
->                       | V98
->                       | V99
->                       | V100
->                       | V101
->                       | V102
->                       | V103
->                       | V104
->                       | V105
->                       | V106
->                       | V107
->                       | V108
->                       | V109
->                       | V110
->                       | V111
->                       | V112
->                       | V113
->                       | V114
->                       | V115
->                       | V116
->                       | V117
->                       | V118
->                       | V119
->                       | V120
->                       | V121
->                       | V122
->                       | V123
->                       | V124
->                       | V125
->                       | V126
->                       | V127;
```


Most common haskell data types can be automatically mapped to the equivalent canonical data type.

There are however a couple of restrictions: data types definitions cannot be mutually recursive and type variables must be of kind *.

So for example, these won't work:

```haskell
-- BAD: f has higher kind
data Free f a = Impure (f (Free f a)) | Pure a

-- BAD: mutually recursive
data Forest a = Nil | Cons (Tree a) (Forest a)
data Tree a = Empty | Node a (Forest a)
```

So now that we have canonical types, what about some practical applications?

#### Safe Deserialisation

To illustrate the problem, consider the two following data types:

The [Cinque Terre](https://en.wikipedia.org/wiki/Cinque_Terre) villages:

```haskell
data CinqueTerre = Monterosso | Vernazza | Corniglia | Manarola | RioMaggiore deriving (Show,Generic,Flat,Model)
```

The traditional Chinese directions:

```haskell
data Direction = North | South | Center | East | West deriving (Show,Generic,Flat,Model)
```

Though their meaning is obviously different they share the same syntactical structure (simple enumerations of 5 values) and most binary serialisation libraries won't be able to distinguish between the two.

To demonstrate this, let's serialise `Center` and `Corniglia`, the third value of each enumeration using the `flat` library.

```haskell
pPrint $ flat Center
-> [ 129 ]
```


```haskell
pPrint $ flat Corniglia
-> [ 129 ]
```


As you can see they have the same binary representation.

We have used the `flat` binary serialisation as it is already a dependency of `zm` (and automatically imported by `ZM`) but the same principle apply to other serialisation libraries (`binary`, `cereal` ..).

Let's go full circle, using `unflat` to decode the value :

```haskell
decoded = unflat . flat
```

```haskell
decoded Center :: Decoded Direction
-> Right Center
```


One more time:

```haskell
decoded Center :: Decoded CinqueTerre
-> Right Corniglia
```


Oops, that's not quite right.

We got our types crossed, `Center` was read back as `Corniglia`, a `Direction` was interpreted as one of the `CinqueTerre`.

To fix this, we convert the value to a `TypedValue`, a value combined with its canonical type:

```haskell
pPrint $ typedValue Center
-> Center :: K170d0e47bef6
```


TypedValues can be serialised as any other value:

```haskell
pPrint <$> (decoded $ typedValue Center :: Decoded (TypedValue Direction))
-> Right Center :: K170d0e47bef6
```


And just as before, we can get things wrong:

```haskell
pPrint <$> (decoded $ typedValue Center :: Decoded (TypedValue CinqueTerre))
-> Right Corniglia :: K170d0e47bef6
```


However this time is obvious that the value is inconsistent with its type, as the `CinqueTerre` data type has a different unique code:

```haskell
pPrint $ absTypeModel (Proxy :: Proxy CinqueTerre)
-> Type:
-> 
-> K747ebaa65778:
-> CinqueTerre
-> 
-> Environment:
-> 
-> CinqueTerre.K747ebaa65778 ≡   Monterosso
->                             | Vernazza
->                             | Corniglia
->                             | Manarola
->                             | RioMaggiore;
```


We can automate this check, with `untypedValue`:

This is ok:

```haskell
untypedValue . decoded . typedValue $ Center :: TypedDecoded Direction
-> Right Center
```


And this is wrong:

```haskell
untypedValue . decoded . typedValue $ Center :: TypedDecoded CinqueTerre
-> Left
->   WrongType
->     { expectedType =
->         TypeCon (AbsRef (SHAKE128_48 116 126 186 166 87 120))
->     , actualType = TypeCon (AbsRef (SHAKE128_48 23 13 14 71 190 246))
->     }
```


### Data Exchange

For an example of using canonical data types as a data exchange mechanism see [top](https://github.com/Quid2/top), the Type Oriented Protocol.


### Haskell Compatibility

Tested with:
  * [ghc](https://www.haskell.org/ghc/) 7.10.3, 8.0.1 and 8.0.2 (x64)
  * [ghcjs](https://github.com/ghcjs/ghcjs)

### Installation

 Get the latest stable version from [hackage](https://hackage.haskell.org/package/zm).

### Acknowledgements

* Contains the following JavaScript library:
 js-sha3 v0.5.1 https://github.com/emn178/js-sha3
 Copyright 2015, emn178@gmail.com
 Licensed under the MIT license:http://www.opensource.org/licenses/MIT

* Includes some functions from the [extra](https://hackage.haskell.org/package/extra) package

### Known Bugs and Infelicities

* The unique codes generated for the data types are not yet final and might change in the final version.
* Instances for parametric data types have to be declared separately (won't work in `deriving`)


### Other Stuff You Might Like

For those who do, you might want to supplement `flat` with  top [ZM - Language independent, reproducible, absolute types](https://github.com/Quid2/zm).
