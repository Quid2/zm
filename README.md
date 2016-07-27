Haskell implementation of canonical, language independent data types.

### How To Use It For Fun and Profit

With `typed` you can derive and manipulate canonical description of (a subset) of Haskell data types.

This can be used, for example:

* in combination with a serialisation library to provide type-safe deserialisation
* for data exchange across different programming languages and sofware systems
* for long term data preservation

#### Canonical Models of Haskell Data Types

For a data type o have a canonical representation, it has to implement the `Model` type class.

Instances for a few common data types (`Bool, Maybe, Tuples, Lists, Ints, Words, String, Text ..`) are already defined (in `Data.Typed.Instances`) and there is `Generics` based support to automatically derive additional instances.

Let's see some code.

We need a couple of GHC extensions:

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
```

Import the library:

```haskell
import Data.Typed
```

We use `absoluteType` to get the canonical type of `Maybe Bool` and `pPrint` to print it nicely:

```haskell
pPrint $ absoluteType (Proxy :: Proxy (Maybe Bool))
(H06b78cab Hac45f78b) -> (Maybe Bool)
Data Types:
H06b78cab ->  Maybe a ≡   Nothing
                        | Just a
Hac45f78b ->  Bool ≡   False
                     | True
```

We can see how the data types `Maybe` and `Bool` have been assigned unique canonical identifiers and how the type `Maybe Bool` is accordingly represented.

Some common classes have rather peculiar custom mappings:

```haskell
pPrint $ absoluteType (Proxy :: Proxy Char)
H1e89074f -> Char
Data Types:
H1c14fcc1 ->  NonEmptyList a ≡   Elem a
                               | Cons a (NonEmptyList a)
H1e89074f ->  Char ≡ Char Word32
Hcbb7765f ->  Word7 ≡   V0
                      | V1
                      | V2
                      | V3
                      | V4
                      | V5
                      | V6
                      | V7
                      | V8
                      | V9
                      | V10
                      | V11
                      | V12
                      | V13
                      | V14
                      | V15
                      | V16
                      | V17
                      | V18
                      | V19
                      | V20
                      | V21
                      | V22
                      | V23
                      | V24
                      | V25
                      | V26
                      | V27
                      | V28
                      | V29
                      | V30
                      | V31
                      | V32
                      | V33
                      | V34
                      | V35
                      | V36
                      | V37
                      | V38
                      | V39
                      | V40
                      | V41
                      | V42
                      | V43
                      | V44
                      | V45
                      | V46
                      | V47
                      | V48
                      | V49
                      | V50
                      | V51
                      | V52
                      | V53
                      | V54
                      | V55
                      | V56
                      | V57
                      | V58
                      | V59
                      | V60
                      | V61
                      | V62
                      | V63
                      | V64
                      | V65
                      | V66
                      | V67
                      | V68
                      | V69
                      | V70
                      | V71
                      | V72
                      | V73
                      | V74
                      | V75
                      | V76
                      | V77
                      | V78
                      | V79
                      | V80
                      | V81
                      | V82
                      | V83
                      | V84
                      | V85
                      | V86
                      | V87
                      | V88
                      | V89
                      | V90
                      | V91
                      | V92
                      | V93
                      | V94
                      | V95
                      | V96
                      | V97
                      | V98
                      | V99
                      | V100
                      | V101
                      | V102
                      | V103
                      | V104
                      | V105
                      | V106
                      | V107
                      | V108
                      | V109
                      | V110
                      | V111
                      | V112
                      | V113
                      | V114
                      | V115
                      | V116
                      | V117
                      | V118
                      | V119
                      | V120
                      | V121
                      | V122
                      | V123
                      | V124
                      | V125
                      | V126
                      | V127
Hf8598df1 ->  Word32 ≡ Word32 (NonEmptyList Word7)
```

Contrary to Haskell, canonical types have no 'magic' built-in types so even something as basic as `Char` has to be defined explicitly.

As you can see `Char` is defined as a tagged `Word32`, defined as a `NonEmptyList` list of `Word7`, defined in turn as an explicit enumeration of all the 128 different values that can fit in 7 bits.

Most common haskell data types can be automatically mapped to the equivalent canonical data type.

There are however a couple of restrictions: data types definitions cannot be mutually recursive and type variables must be of kind *.

So for example, these won't work:

````haskell
-- BAD: f has higher kind
data Free = Impure (f (Free f a)) | Pure a

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

To demonstrate this let's serialise a value using the `flat` binary serialisation library.

```haskell
flat Center
"\129"
```

We use `flat` as it is already a dependency of `typed` (and automatically imported by `Data.Typed`) but the same principle apply to other serialisation libraries (`binary`, `cereal` ..).

Let's go full circle, using `unflat` to decode the value :

```haskell
unflat (flat Center) :: Decoded Direction
Right Center
```

One more time:

```haskell
unflat (flat Center) :: Decoded CinqueTerre
Right Corniglia
```

Oops, that's not quite right.

We got our types crossed, a Direction was interpreted as one of the CinqueTerre.

To fix this, we convert the value to a `TypedValue`, a value combined with its canonical type:

```haskell
pPrint $ typedValue Center
Center :: H844cade3
```

TypedValues can be serialised as any other value:

```haskell
pPrint <$> (unflat $ flat $ typedValue Center :: Decoded (TypedValue Direction))
Right Center :: H844cade3
```

And just as before, we can get things wrong:

```haskell
pPrint <$> (unflat $ flat $ typedValue Center :: Decoded (TypedValue CinqueTerre))
Right Corniglia :: H844cade3
```

However this time is obvious that the value is inconsistent with its type, as the `CinqueTerre` data type has a different unique code:

```haskell
pPrint $ absoluteType (Proxy :: Proxy CinqueTerre)
Hf517568b -> CinqueTerre
Data Types:
Hf517568b ->  CinqueTerre ≡   Monterosso
                            | Vernazza
                            | Corniglia
                            | Manarola
                            | RioMaggiore
```

We can automate this check, with `untypedValue`:

This is ok:

```haskell
untypedValue . unflat . flat . typedValue $ Center :: Decoded Direction
Right Center
```

And this is wrong:

```haskell
untypedValue . unflat . flat . typedValue $ Center :: Decoded CinqueTerre
Left "Was expecting type:\n Hf517568b \n\nBut the data has type:\n H844cade3"
```

### Data Exchange

For an example of using canonical data types as a data exchange mechanism see [top](https://github.com/tittoassini/top), the typed oriented protocol.


### Installation

Install as part of the [quid2](https://github.com/tittoassini/quid2) project.

### Known Bugs and Infelicities

* Instances for parametric data types have to be declared separately (won't work in `deriving`)
* Messy source code
