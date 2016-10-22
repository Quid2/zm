
Haskell implementation of canonical, language independent data types.

### How To Use It For Fun and Profit

With `typed` you can derive and manipulate canonical description of (a subset) of Haskell data types.

This can be used, for example:

* in combination with a serialisation library to provide type-safe deserialisation
* for data exchange across different programming languages and software systems
* for long term data preservation

#### Canonical Models of Haskell Data Types

For a data type to have a canonical representation, it has to implement the `Model` type class.

Instances for a few common data types (Bool, Maybe, Tuples, Lists, Ints, Words, String, Text ..) are already defined (in `Data.Typed.Instances`) and there is `Generics` based support to automatically derive additional instances.

Let's see some code.

We need a couple of GHC extensions:

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
```

Import the library:

```haskell
import Data.Typed
```

We use `absoluteType` to get the canonical type of `Maybe Bool` and `pPrint` to print is nicely:

```haskell
pPrint $ absoluteType (Proxy :: Proxy (Maybe Bool))
Scfe64fd7a36f S71aa1bacd97c -> Maybe Bool
```

```haskell
Data Types:
S71aa1bacd97c ->  Bool ≡   False
        | True
Scfe64fd7a36f ->  Maybe a ≡   Nothing
           | Just a
```

We can see how the data types `Maybe` and `Bool` have been assigned unique canonical identifiers and how the type `Maybe Bool` is accordingly represented.

Contrary to Haskell, `typed` has no 'magic' built-in types so even something as basic as a `Char` or a `Word` have to be defined explicitly.

For example, a `Word7` (an unsigned integer of 7 bits length) is defined as an explicit enumeration of all the 128 different values that can fit in 7 bits:

```haskell
pPrint $ absoluteType (Proxy :: Proxy Word7)
Sec001a6a1de3 -> Word7
```

```haskell
Data Types:
Sec001a6a1de3 ->  Word7 ≡   V0
         | V1
         | V2
         | V3
         | V4
...
         | V123
         | V124
         | V125
         | V126
         | V127
```


A `Word32` can then be defined as a `NonEmptyList` list of `Word7`s (a definition equivalent to the [Base 128 Varints encoding](https://developers.google.com/protocol-buffers/docs/encoding#varints)).

```haskell
pPrint $ absoluteType (Proxy :: Proxy Word32)
Sf4543433b7ad -> Word32
```

```haskell
Data Types:
S0ccddce51672 ->  NonEmptyList a ≡   Elem a
                  | Cons a (NonEmptyList a)
Sb629b033a6ea ->  LeastSignificantFirst a ≡ LeastSignificantFirst a
Sd69caaba3e20 ->  MostSignificantFirst a ≡ MostSignificantFirst a
Sec001a6a1de3 ->  Word7 ≡   V0
         | V1
         | V2
         | V3
         | V4
...
         | V123
         | V124
         | V125
         | V126
         | V127
```

```haskell
Sf016cfe5d117 ->  Word ≡ Word (LeastSignificantFirst (NonEmptyList (MostSignificantFirst Word7)))
Sf4543433b7ad ->  Word32 ≡ Word32 Word
```

And finally a `Char` can be defined as a tagged `Word32`:

```haskell
pPrint $ absoluteType (Proxy :: Proxy Char)
S4d90d0005d6a -> Char
```

```haskell
Data Types:
S0ccddce51672 ->  NonEmptyList a ≡   Elem a
                  | Cons a (NonEmptyList a)
S4d90d0005d6a ->  Char ≡ Char Word32
Sb629b033a6ea ->  LeastSignificantFirst a ≡ LeastSignificantFirst a
Sd69caaba3e20 ->  MostSignificantFirst a ≡ MostSignificantFirst a
Sec001a6a1de3 ->  Word7 ≡   V0
         | V1
         | V2
         | V3
         | V4
...
         | V123
         | V124
         | V125
         | V126
         | V127
```

```haskell
Sf016cfe5d117 ->  Word ≡ Word (LeastSignificantFirst (NonEmptyList (MostSignificantFirst Word7)))
Sf4543433b7ad ->  Word32 ≡ Word32 Word
```

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

To demonstrate this, let's serialise `Center` and `Corniglia`, the third value of each enumeration.

```haskell
flat Center
"\129"
```

```haskell
flat Corniglia
"\129"
```

As you can see they have the same binary representation.

We have used the `flat` binary serialisation as it is already a dependency of `typed` (and automatically imported by `Data.Typed`) but the same principle apply to other serialisation libraries (`binary`, `cereal` ..).

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

We got our types crossed, `Center` was read back as `Corniglia`, a Direction was interpreted as one of the CinqueTerre.

To fix this, we convert the value to a `TypedValue`, a value combined with its canonical type:

```haskell
pPrint $ typedValue Center
Center :: Sabbc0db9ba75
```

TypedValues can be serialised as any other value:

```haskell
pPrint <$> (unflat $ flat $ typedValue Center :: Decoded (TypedValue Direction))
Right Center :: Sabbc0db9ba75
```

And just as before, we can get things wrong:

```haskell
pPrint <$> (unflat $ flat $ typedValue Center :: Decoded (TypedValue CinqueTerre))
Right Corniglia :: Sabbc0db9ba75
```

However this time is obvious that the value is inconsistent with its type, as the `CinqueTerre` data type has a different unique code:

```haskell
pPrint $ absoluteType (Proxy :: Proxy CinqueTerre)
S0d596b699264 -> CinqueTerre
```

```haskell
Data Types:
S0d596b699264 ->  CinqueTerre ≡   Monterosso
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
Left "Was expecting type:\n S0d596b699264 \n\nBut the data has type:\n Sabbc0db9ba75"
```

### Data Exchange

For an example of using canonical data types as a data exchange mechanism see [top](https://github.com/tittoassini/top), the Type Oriented Protocol.
<!--
### Long Term Data Preservation

For an example of using canonical data types as a long term data preservation mechanism see [timeless](https://github.com/tittoassini/timeless).

Inspect the data to figure out its type dynamically

So far so good but what if we lose the definitions of our data types?

Two ways:
-- save the full canonical definition of the data with the data itself or
-- save the def in the cloud so that it can be shared

When we save

Better save them for posterity:

sv = saveTypeIn theCloud (Couple One Tre)

The type has been saved, with all its dependencies.
TypeApp (TypeApp (TypeCon (CRC16 91 93)) (TypeCon (CRC16 79 130))) (TypeCon (CRC16 65 167))

Now that they are safe in the Cloud we can happily burn our code
in the knowledge that when we are presented with a binary of unknown type
we can always recover the full definition of our data.

PUT BACK dt = e2 >>= recoverTypeFrom theCloud

What if we have no idea of what is the type

instance (Flat a , Flat b) => Flat (CoupleB a b)

t = ed False >> ed Tre >> ed (Couple (CoupleB True Uno One) Three)
ed = pp . unflatDynamically . flat . typedValue


We can now use it to define a hard-wired decoder

Or use a dynamic decder to directly show the value.

The final system will also keep track of the documentation that comes with the types to give you a fully human understandable description of the data.
-->

### Installation

It is not yet on [hackage](https://hackage.haskell.org/) but you can still use it in your [stack](https://docs.haskellstack.org/en/stable/README/) projects by adding a reference to its github location under the 'packages' section:

````
packages:
- location:
   git: https://github.com/tittoassini/typed
   commit: 5cb0f72
````

### Compatibility

Tested with [ghc](https://www.haskell.org/ghc/) 7.10.3 and 8.0.1.

### Known Bugs and Infelicities

* The unique codes generated for the data types are not yet final and will change in the next version.
* Instances for parametric data types have to be declared separately (won't work in `deriving`)
* Messy source code
