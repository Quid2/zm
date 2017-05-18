
[![Build Status](https://travis-ci.org/tittoassini/zm.svg?branch=master)](https://travis-ci.org/tittoassini/zm) [![Hackage version](https://img.shields.io/hackage/v/zm.svg)](http://hackage.haskell.org/package/zm)

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
-> K306f1981b41c:
->  Bool ≡   False
->         | True
-> 
-> Kda6836778fd4:
->  Maybe a ≡   Nothing
->            | Just a
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
-> Kf4c946334a7e:
->  Word7 ≡   V0
->          | V1
->          | V2
->          | V3
->          | V4
-> ...
->          | V123
->          | V124
->          | V125
->          | V126
->          | V127
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
-> K20ffacc8f8c9:
->  LeastSignificantFirst a ≡ LeastSignificantFirst a
-> 
-> K74e2b3b89941:
->  MostSignificantFirst a ≡ MostSignificantFirst a
-> 
-> Kbf2d1c86eb20:
->  NonEmptyList a ≡   Elem a
->                   | Cons a (NonEmptyList a)
-> 
-> Kf92e8339908a:
->  Word ≡ Word (LeastSignificantFirst (NonEmptyList (MostSignificantFirst Word7)))
-> 
-> K2412799c99f1:
->  Word32 ≡ Word32 Word
-> 
-> Kf4c946334a7e:
->  Word7 ≡   V0
->          | V1
->          | V2
->          | V3
->          | V4
-> ...
->          | V123
->          | V124
->          | V125
->          | V126
->          | V127
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
-> K066db52af145:
->  Char ≡ Char Word32
-> 
-> K20ffacc8f8c9:
->  LeastSignificantFirst a ≡ LeastSignificantFirst a
-> 
-> K74e2b3b89941:
->  MostSignificantFirst a ≡ MostSignificantFirst a
-> 
-> Kbf2d1c86eb20:
->  NonEmptyList a ≡   Elem a
->                   | Cons a (NonEmptyList a)
-> 
-> Kf92e8339908a:
->  Word ≡ Word (LeastSignificantFirst (NonEmptyList (MostSignificantFirst Word7)))
-> 
-> K2412799c99f1:
->  Word32 ≡ Word32 Word
-> 
-> Kf4c946334a7e:
->  Word7 ≡   V0
->          | V1
->          | V2
->          | V3
->          | V4
-> ...
->          | V123
->          | V124
->          | V125
->          | V126
->          | V127
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
-> K747ebaa65778:
->  CinqueTerre ≡   Monterosso
->                | Vernazza
->                | Corniglia
->                | Manarola
->                | RioMaggiore
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

For an example of using canonical data types as a data exchange mechanism see [top](https://github.com/tittoassini/top), the Type Oriented Protocol.


### Haskell Compatibility

Tested with:
  * [ghc](https://www.haskell.org/ghc/) 7.10.3, 8.0.1 and 8.0.2 (x64)
  * [ghcjs](https://github.com/ghcjs/ghcjs)

### Installation

 Get the latest stable version from [hackage](https://hackage.haskell.org/package/zm).

### Acknowledgements
 Contains the following JavaScript library:

 js-sha3 v0.5.1 https://github.com/emn178/js-sha3

 Copyright 2015, emn178@gmail.com

 Licensed under the MIT license:http://www.opensource.org/licenses/MIT

### Known Bugs and Infelicities

* The unique codes generated for the data types are not yet final and might change in the final version.
* Instances for parametric data types have to be declared separately (won't work in `deriving`)
