{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Parse ZM data type definitions
module ZM.Parser.Env
  ( parseADTs
  , parseADTsWith
  ) where

import           ZM.Parser.ADT

import           Data.Either
import           Data.List

-- import           Data.Maybe
import           ZM.Parser.Types

-- import qualified ZM.Parser.Parser as P
import           ZM.Parser.Util
import           ZM.Pretty

import           Control.Monad
import           Data.Bifunctor

--import Data.Either.Extra
-- import           Data.Either.Validation
import           Data.Foldable
import qualified Data.Map        as M
import           Data.Model
import           Data.Word
import           ZM.Abs
import           ZM.Types

-- tst = putStr . prettyShow . parseADTs
-- ok = putStr . prettyShow . (\(Right a) -> a) . parseADTs
-- import           ZM.Util
{-|
Parse a (possibly empty) closed group of ADT declarations.

>>> tst = putStr . prettyShow . parseADTs
>>> ok = putStr . prettyShow . (\(Right a) -> a) . parseADTs

Empty declarations are ok:

>>> parseADTs ""
Right (fromList [])

Line comments are introduced by `--`:

>>> parseADTs "-- Nothing to see here"
Right (fromList [])

ZM datatype definitions are very similar to Haskell's.

There is no initial `data` keyword, so to define an empty data type just state its name:

>>> ok "Void"
Void.Kf4165614d4f2 ;

Every data type has an (almost) unique absolute reference, for `Void` is `Kf4165614d4f2`.

The reference can be explicitly stated:

>>> ok "Void.Kf4165614d4f2"
Void.Kf4165614d4f2 ;

Cannot have names wih multiple parts:
>>> tst "a.really.bad.adt.name"
Left ["unexpected 'r' expecting 'K'"@(0:2)]

Names of both data types and constructors start with a unicode letter (upper or lowercase) followed by zero or more letters and numbers (but no symbols):

>>> ok "bool"
bool.K4badcbf6aefb ;

>>> ok "是 -- chinese characters are fine too!"
是.K086ea5f1306b ;

Unicode symbols are not allowed:

>>> tst "<>"
Left ["unexpected '<' expecting end of input or letter"@(0:0)]

A data type can have zero or more constructors, separated by a `|`:

>>> ok "bool = false | true"
bool.Kb61fc3993fd1 ≡   false
                     | true;

Constructor names must be unique:

>>> tst "T = C1 | C1 | C2 | C3 | C3"
Left ["Duplicated constructor: C1"@(0:9-10),
      "Duplicated constructor: C3"@(0:24-25)]

Blank space is allowed, no special indentation is required:

>>> ok "是不是 ≡   是\n |\n \n \t 不是;"
是不是.Ka0df1ad8aa92 ≡   是
...               | 不是;

Constructors can have named fields.

Field types can be introduced by either ":" or "::"

>>> tst "Bool = False|True;Id = A | B;Switch = Switch {name::Id,state:Bool}"
Right Bool.K306f1981b41c ≡   False
                           | True;
<BLANKLINE>
      Id.K7e20ca15902f ≡   A
                         | B;
<BLANKLINE>
      Switch.Kcd5467075768 ≡   Switch {name :: Id.K7e20ca15902f,
                                       state :: Bool.K306f1981b41c};

Multiple fields with a single type signature are not supported

>>> tst "Id = A | B;Ids = Ids {id0,id1:Id}"
Left ["unexpected \",i\" expecting \"::\", ':', '_', or alphanumeric character"@(0:25)]

A data type can have zero or more type variables:

>>> ok "Maybe a = Nothing | Just a"
Maybe.Kda6836778fd4 a ≡   Nothing
                        | Just a;

Variable names must be unique and different from the data type name:

>>> tst "list a list a = nil"
Left ["Duplicated type name: list"@(0:7-10),
      "Duplicated type name: a"@(0:12)]

Contructor names must be unique
>>> tst "DupConstructors = a | a | b | c | b"
Left ["Duplicated constructor: a"@(0:22),
      "Duplicated constructor: b"@(0:34)]

Phantom type variables (variables that are declared but not used in the constructors) are ok:

>>> tst $ "P phantom ≡ P"
Right P.K76cc89d4c46f a ≡   P;


Self-recursion is ok:

>>> tst $ "List a = Cons a (List a) | Nil"
Right List.Kb8207ed4e169 a ≡   Cons a (List.Kb8207ed4e169 a)
                             | Nil;

Multiple data types definitions are separated by semicolumns:

>>> tst $ "Void;Bool = False | True"
Right Bool.K306f1981b41c ≡   False
                           | True;
<BLANKLINE>
      Void.Kf4165614d4f2 ;

Source must be syntactically correct:

>>> tst "Bad !"
Left ["unexpected '!' expecting '.', ';', '=', '\8801', end of input, or letter"@(0:4)]

>>> tst "a = |"
Left ["unexpected '|' expecting ';', end of input, or letter"@(0:4)]

>>> tst "= ="
Left ["unexpected '=' expecting end of input or letter"@(0:0)]

>>> tst "a b.d = c.f"
Left ["unexpected '.' expecting ';', '=', '_', '\8801', alphanumeric character, end of input, or letter"@(0:3)]

All references must be resolvable.

Local references must resolve to an adt variable or a locally named type:

>>> tst "T = T a Bool"
Left ["Reference to unknown type: a"@(0:6),
      "Reference to unknown type: Bool"@(0:8-11)]

If multiple type declarations with the same name are present, only the last one is used:

>>> tst $ "Void;A = A Void;Void = V;B = B Void"
Right A.K3a8a7ea9f77c ≡   A Void.K498d6de22d87;
<BLANKLINE>
      B.K531b1a37d8d8 ≡   B Void.K498d6de22d87;
<BLANKLINE>
      Void.K498d6de22d87 ≡   V;

But you can add an (unckecked) K suffix to distinguish them:

>>> tst $ "Void.Kf4165614d4f2;A = VV Void.K498d6de22d87 | V Void.Kf4165614d4f2;Void.K498d6de22d87 = V;"
Right A.K3d3bca29bc26 ≡   VV Void.K498d6de22d87
                        | V Void.Kf4165614d4f2;
<BLANKLINE>
      Void.Kf4165614d4f2 ;
<BLANKLINE>
      Void.K498d6de22d87 ≡   V;

Type constructors are fully applied:

>>> tst "Either a b = Left a | Right b;T a = T (Either a)"
Left ["Incorrect application of Either, should have 2 parameters but has 1"@(0:39-44)]

>>> tst "Bool = False;Maybe a = Just a | Nothing;Either a b = Left a | Right b;G g = g g;T a = T (Either (Maybe a) (Either (Maybe a) Bool Maybe))"
Left ["Incorrect application of Either, should have 2 parameters but has 3"@(0:107-112),
      "Incorrect application of Maybe, should have 1 parameters but has 0"@(0:129-133)]

No higher kinds (all variables are assumed to be fully applied data types):

>>> tst "Free f a = Pure a | Free (f (Free f a))"
Left ["Incorrect application of f, should have 0 parameters but has 1"@(0:26)]

>>> tst "Fix f a = Fix (f a)"
Left ["Incorrect application of f, should have 0 parameters but has 1"@(0:15)]

>>> tst "FixR f a = FixR (f (f a))"
Left ["Incorrect application of f, should have 0 parameters but has 1"@(0:17),
      "Incorrect application of f, should have 0 parameters but has 1"@(0:20)]

No mutually recursive definitions:

>>> tst "A = A B;B = B A;C = C D;D = D C A"
Left ["Found mutually recursive types: [D, C]"@(0:22),
      "Found mutually recursive types: [D, C]"@(0:16),
      "Found mutually recursive types: [B, A]"@(0:6),
      "Found mutually recursive types: [B, A]"@(0:0)]

We try to capture as many errors as possible at the same time:

>>> tst "T a a b T = C1 | C1 | C2 | C3 | C2 | T "
Left ["Duplicated type name: a"@(0:4),
      "Duplicated type name: T"@(0:8),
      "Duplicated constructor: C1"@(0:17-18),
      "Duplicated constructor: C2"@(0:32-33)]


>>> tst "List List ≡ Nil"
Left ["Duplicated type name: List"@(0:5-8)]

-}
parseADTs :: String -> Either [AtError] AbsEnv
parseADTs = parseADTsWith M.empty

{-|
Parse a (possibly empty) group of ADT declarations, in the context of an environment.


An environment with definitions for Bool and List:

>> prettyShow $ absEnv (Proxy :: Proxy [Bool])

>> parseADTsWith (absEnv (Proxy :: Proxy [Bool])) $ "T = T (List.Kb8cd13187198 Bool.K306f1981b41c)"

>> :{
      let env = absEnv (Proxy :: Proxy [Bool])
      in putStr . prettyShow . parseADTsWith (absEnv (Proxy :: Proxy [Bool])) $
         unlines [ " -- We refer to two external absolute types"
                  ,"T = T (List.Kb8cd13187198 Bool.K306f1981b41c)"
                  ]
:}
Right Bool.K306f1981b41c ≡   False
                            | True;
...
      List.Kb8cd13187198 a ≡   Nil
                              | Cons a (List.Kb8cd13187198 a);
...
      T.K63f0354d9548 ≡   T (List.Kb8cd13187198 Bool.K306f1981b41c);

If a type is defined locally it should be referred to with a local reference, not an absolute one:
NOT REALLY.

>> putStr . prettyShow . parseADTsWith (absEnv (Proxy :: Proxy Bool)) $ "T = T Bool.K306f1981b41c;Bool = False | True"


Remote references to correct types are retrieved and checked:

>> tst "T = T Bool.K306f1981b41c"
BAD: Left ["Reference to unknown type: Bool.K306f1981b41c"@(0:6-23)]

-}
parseADTsWith :: AbsEnv -> String -> Either [AtError] AbsEnv
parseADTsWith absEnv =
  either
    (\e -> Left [e])
    (makeADTs >=> kindCheckWith absEnv >=> toAbsEnvWith absEnv) .
  parseDoc adts --makeEnv >=> toAbsEnvWith absEnv
  where
    kindCheckWith absEnv relEnv =
      case concatMap atk . kindErrors $ addAbsEnv absEnv relEnv of
        []   -> Right relEnv
        errs -> Left errs
    atk ::
         ZMError (Either (Label (Range, Word8) Identifier) (At (TypeName Identifier)))
      -> [At String]
    atk e =
      let s = prettyShow (either (pPrint . object) (pPrint . object) <$> e)
       in map (either (\t -> Label (fst $ label t) s) (\t -> Label (label t) s)) $
          toList e
    addAbsEnv absEnv relEnv =
      relEnv `M.union`
      (M.fromList .
       map
         (\radt@(_, adt) ->
            ( keyOf radt
            , ADT (at0 (declName adt)) (declNumParameters adt) Nothing)) .
       M.toList $
       absEnv)

-- makeEnv ::
--      [ADTParts]
--   -> Either [AtError] (M.Map (At (TypeName Identifier)) (ADT AtId AtId (TypeRef2 (Label ( Range
--                                                                                         , Word8) Identifier) (At (TypeName Identifier)))))
-- makeEnv = makeADTs
toAbsEnvWith ::
     Convertible n Identifier
  => AbsEnv
  -> M.Map (At (TypeName Identifier)) (ADT n n (TypeRef2 (Label (t, Word8) t1) (At (TypeName Identifier))))
  -> Either [At String] AbsEnv
toAbsEnvWith absEnv
  --let extEnv = relEnv `M.union` (M.fromList . map (\radt -> (keyOf radt,ADT (Label (Range 0 0 0) (TypeName "" Nothing)) 0 Nothing)) .  M.toList $ absEnv)
  -- in first (concatMap ate) . relToAbsEnvWith absEnv . ((asRef <$>) <$>) $ relEnv
  --in first (concatMap ate) . relToAbsEnv . ((asRef <$>) <$>) $ extEnv
 = first (concatMap ate) . relToAbsEnvWith absEnv . ((asRef <$>) <$>)
  where
    ate :: ZMError (At (TypeName Identifier)) -> [At String]
    ate e =
      let s = prettyShow (object <$> e)
       in map (\t -> Label (label t) s) $ toList e

instance KeyOf (At (TypeName Identifier)) (AbsRef, AbsADT) where
  keyOf (ref, adt) = at0 (asTypeName (Just $ declName adt) (Just ref))

at0 :: a -> Label Range a
at0 = Label (Range 0 0 0)

-- FIX: to be moved in main model
type TRef
   = TypeRef2 (Label (Range, Word8) Identifier) (At (TypeName Identifier))

makeADTs ::
     [ADTParts]
  -> Either [AtError] (M.Map (At (TypeName Identifier)) (ADT AtId AtId TRef))
makeADTs adts =
  let eadts = map makeADT adts
      errors = concat $ lefts eadts
   in if null errors
        then Right . M.fromList $ rights eadts
        else Left errors

makeADT ::
     ADTParts -> Either [AtError] (At (TypeName Identifier), ADT AtId AtId TRef)
makeADT adt =
  let errors = uniqueLocalTypeNames adt ++ uniqueConstrNames adt
      --cons = (contree . constrs $ adt) >>= traverse (varOrName (M.fromList (zip (map object $ vars adt) [0..])))
      cons =
        (varOrName (M.fromList (zip (map object $ vars adt) [0 ..])) <$>) <$>
        (contree . constrs $ adt)
   in if null errors
        then Right
               ( name adt
               , ADT
                   { declName = localName <$> name adt
                   , declNumParameters = fromIntegral (length (vars adt))
                   , declCons = cons
                   })
        else Left errors
  -- Variable names take precedence over type declarations
  where
    varOrName :: M.Map Identifier Word8 -> At (TypeName Identifier) -> TRef
  -- varOrName vs r@(Label l (TypeName n (Just _))) = Ext2 r
  -- varOrName vs r@(Label l (TypeName n Nothing)) =
    varOrName vs r@(Label l tn)
      | hasRef tn = Ext2 r
      | otherwise =
        let n = typeNameName tn
         in case M.lookup n vs of
              Nothing -> Ext2 r -- . asQualName <$> n
              Just i  -> Var2 (Label (l, i) n)

asRef :: TypeRef2 (Label (a, Word8) a1) name -> TypeRef name
asRef (Ext2 r)                = TypRef r
asRef (Var2 (Label (_, i) _)) = TypVar i -- (Label l (TypeName n Nothing))
  -- varOrName vs r@(At l (TypeName n (Just _))) = TypRef r
  -- varOrNameT vs r@(At l (TypeName n Nothing)) =
  --    case M.lookup n vs of
  --          -- Nothing -> Just $ TypRef . asQualName <$> n
  --          Just i -> Just $ TypVar i

--uniqueADTName adts = dupErrors "data type declaration" $ map name adts
-- Locally defined type names (the adt name plus the variables names) must be unique
uniqueLocalTypeNames :: ADTParts -> [Label Range String]
uniqueLocalTypeNames adt =
  dupErrors "type name" ((localName <$> name adt) : vars adt)

uniqueConstrNames :: ADTParts -> [Label Range String]
uniqueConstrNames = dupErrors "constructor" . map fst . constrs

--dupError kind = testErrors dups (\ds -> [unwords ["Duplicated",kind++":",unwords ds]])
-- dupErrors :: String -> [AtName] -> [AtError]
dupErrors :: (Eq (f a), Pretty a, Functor f) => [Char] -> [f a] -> [f String]
dupErrors kind vs =
  (((\n -> unwords ["Duplicated", kind ++ ":", prettyShow n]) <$>) <$>) $
  dups vs

dups :: Eq a => [a] -> [a]
dups ls = ls \\ nub ls
-- dups :: Eq a => [a] -> [a]
-- dups ls = deleteFirstsBy atEq ls (nubBy atEq ls)
-- -- makeADT :: String -> [String] -> [(String, Fields String String)] -> Either Errors (QualName, ADT String String (TypeRef QualName))
-- makeADT :: ADTParts -> Either Errors (QualName, ADT AName AName (TypeRef QualName))
-- -- makeADT :: ADTParts -> Either Errors (QualName, ADT String String (TypeRef QualName))
-- makeADT adt =
--   let
--       eqn = parseQN (name adt)
--       econstrs = case contree (constrs adt) of
--                    Nothing -> Success Nothing
--                    Just ct -> Just <$> traverse (varOrName (M.fromList (zip (vars adt) [0..]))) ct
--       nameClashError = if (name adt) `elem` vars adt then Failure ["data type name equal to a variable"] else Success ()
--       constrNames = map fst (constrs adt)
--       constrDupErrors = dupErrors "constructors" constrNames
--       varDupErrors = dupErrors "variables" (vars adt)
--       constrNameErrors = idErrors constrNames
--       varNameErrors = idErrors (vars adt)
--   in validationToEither $
--      (\qn constrs -> (qn,ADT {declName=locName <$> qn
--                              ,declNumParameters=fromIntegral (length (vars adt))
--                              ,declCons = constrs})) <$> eqn <*> econstrs <* nameClashError <* varDupErrors <* constrDupErrors <* constrNameErrors <* varNameErrors
--idErrors :: [String] -> Validation Errors [Identifier]
--idErrors = traverse (asIdentifier . object)
--dupError :: String -> [String] -> Validation Errors [String]
--dupError kind = testErrors dups (\ds -> [unwords ["Duplicated",kind++":",unwords ds]])
-- dupErrors kind vs =
--   (((\n -> unwords ["Duplicated", kind, n]) <$>) <$>) $ dups vs
-- dupErrors kind vs = ((\n -> unwords ["Duplicated",kind,object n]) <$>) $ dups vs
-- testErrors :: Foldable t => (a1 -> t a2) -> (t a2 -> e) -> a1 -> Validation e a1
-- testErrors tst onErrs i =
--   let errs = tst i
--   in if null errs
--        then Success i
--        else Failure . onErrs $ errs
-- dups :: Eq a => [a] -> [a]
-- dups ls = ls \\ nub ls
-- varOrName ::
--   M.Map String Word8
--   -> String -> Validation Errors (TypeRef QualName)
-- varOrName vs n =
--   case M.lookup n vs of
--     Nothing -> TypRef <$> parseQN n
--     Just i -> pure $ TypVar i -- const (TypVar i) <$> identifier n
--parseQN :: String -> Either Errors QualName
-- parseQN :: String -> Validation Errors QualName
--parseQN = eitherToValidation . convertResultToErrors . safeConvert --asIdentifier :: String -> Either Errors Identifier
--asIdentifier = convertResultToErrors . safeConvert
--readDataTypes :: String -> Either Errors AbsEnv
-- readDataTypes src = undefined
--   parse src >>= relToAbsEnv >>= validationToEither . testErrors kindErrors id
-- -- parse :: String -> Either String AbsEnv
-- parse ::
--      String
--   -> Either [String] (M.Map QualName (ADT String String (TypeRef QualName)))
-- parse src = M.fromList <$> (alexScanTokens >=> parser) src
