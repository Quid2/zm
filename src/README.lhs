Haskell implementation of canonical, language independent data types.

 ### How To Use It For Fun and Profit

With `typed` you can derive and manipulate canonical description of (a subset) of Haskell data types.

This can be used, for example:

* in combination with a serialisation library to provide type-safe deserialisation
* for data exchange across different programming languages and sofware systems
* for long term data preservation

 #### Canonical Models of Haskell Data Types

For a data type to have a canonical representation, it has to implement the `Model` type class.

Instances for a few common data types (Bool, Maybe, Tuples, Lists, Ints, Words, String, Text ..) are already defined (in `Data.Typed.Instances`) and there is `Generics` based support to automatically derive additional instances.

Let's see some code.

We need a couple of GHC extensions:

> {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

Import the library:

> import Data.Typed

We use `absoluteType` to get the canonical type of `Maybe Bool` and `pPrint` to print is nicely:

> b2 = pPrint $ absoluteType (Proxy :: Proxy (Maybe Bool))

We can see how the data types `Maybe` and `Bool` have been assigned unique canonical identifiers and how the type `Maybe Bool` is accordingly represented.

Some common classes have rather peculiar custom mappings:

> b3 = pPrint $ absoluteType (Proxy :: Proxy Char)

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

> data CinqueTerre = Monterosso | Vernazza | Corniglia | Manarola | RioMaggiore deriving (Show,Generic,Flat,Model)

The traditional Chinese directions:

> data Direction = North | South | Center | East | West deriving (Show,Generic,Flat,Model)

Though their meaning is obviously different they share the same syntactical structure (simple enumerations of 5 values) and most binary serialisation libraries won't be able to distinguish between the two.

To demonstrate this let's serialise a value using the `flat` binary serialisation library.

> e1 = flat Center

We use `flat` as it is already a dependency of `typed` (and automatically imported by `Data.Typed`) but the same principle apply to other serialisation libraries (`binary`, `cereal` ..).

Let's go full circle, using `unflat` to decode the value :

> d1 = unflat (flat Center) :: Decoded Direction

One more time:

> d2 = unflat (flat Center) :: Decoded CinqueTerre

Oops, that's not quite right.

We got our types crossed, a Direction was interpreted as one of the CinqueTerre.

To fix this, we convert the value to a `TypedValue`, a value combined with its canonical type:

> t3 = pPrint $ typedValue Center

TypedValues can be serialised as any other value:

> t4 = pPrint <$> (unflat $ flat $ typedValue Center :: Decoded (TypedValue Direction))

And just as before, we can get things wrong:

> t5 = pPrint <$> (unflat $ flat $ typedValue Center :: Decoded (TypedValue CinqueTerre))

However this time is obvious that the value is inconsistent with its type, as the `CinqueTerre` data type has a different unique code:

> b33 = pPrint $ absoluteType (Proxy :: Proxy CinqueTerre)

We can automate this check, with `untypedValue`:

This is ok:

> t6 = untypedValue . unflat . flat . typedValue $ Center :: Decoded Direction

And this is wrong:

> t7 = untypedValue . unflat . flat . typedValue $ Center :: Decoded CinqueTerre

> t8 = flat . 

 ### Data Exchange

For an example of using canonical data types as a data exchange mechanism see [top](https://github.com/tittoassini/top), the typed oriented protocol.

 ### Long Term Data Preservation

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

 ### Installation

Install as part of the [quid2](https://github.com/tittoassini/quid2) project.

 ### Known Bugs and Infelicities

* Instances for parametric data types have to be declared separately (won't work in `deriving`)
* Messy source code

