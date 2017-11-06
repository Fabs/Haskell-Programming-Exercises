> import Data.List

Multiple choice

1. c
2. c
3. a
4. c
5. a

Does it typecheck?

1. No, because it doesn't instantiate show.

   Chapter Exercises.lhs:16:34: error:
       • No instance for (Show Person) arising from a use of ‘show’
       • In the first argument of ‘putStrLn’, namely ‘(show person)’
         In the expression: putStrLn (show person)
         In an equation for ‘printPerson’:
             printPerson person = putStrLn (show person)
      |
   16 | > printPerson person = putStrLn (show person)
      |

The fixed code is:

> data Person = Person Bool deriving Show

> printPerson :: Person -> IO ()
> printPerson person = putStrLn (show person)


2. Yes. I think it should typecheck. However, I was wrong because x needs to instantiate Eq for == to work.

   Chapter Exercises.lhs:36:21: error:
       • No instance for (Eq Mood) arising from a use of ‘==’
       • In the expression: x == Woot
         In the expression: if x == Woot then Blah else x
         In an equation for ‘settleDown’:
             settleDown x = if x == Woot then Blah else x
      |
   36 | > settleDown x = if x == Woot
      |

The fixed code is:

> data Mood = Blah
>           | Woot deriving (Show, Eq)

> settleDown x = if x == Woot
>                  then Blah
>                  else x

3. a) Blah and Woot
   b) There is no instance of a Num for Mood

      λ settleDown 9

      <interactive>:9:12: error:
          • No instance for (Num Mood) arising from the literal ‘9’
          • In the first argument of ‘settleDown’, namely ‘9’
            In the expression: settleDown 9
            In an equation for ‘it’: it = settleDown 9
    c) Mood does not instantiate Ord so it isn't going to be able to understand the > function

       λ Blah > Woot

       <interactive>:10:1: error:
           • No instance for (Ord Mood) arising from a use of ‘>’
           • In the expression: Blah > Woot
             In an equation for ‘it’: it = Blah > Woot

4. Yes. It should typecheck.

> type Subject = String
> type Verb = String
> type Object = String
>
> data Sentence =
>   Sentence Subject Verb Object
>   deriving (Eq, Show)
>
> s1 = Sentence "dogs" "drool"
> s2 = Sentence "Julie" "loves" "dogs"

Given a datatype declaration, what can we do?

> data Rocks = Rocks String deriving (Eq, Show)
> data Yeah = Yeah Bool deriving (Eq, Show)
> data Papu = Papu Rocks Yeah deriving (Eq, Show)

Rocks, Yeah and Papu can be compared to be equal or not and it can be represented as a string.


Which of the following will typecheck? For the ones that don't typecheck, why don't they?

1. Needs constructors for Rocks and Yeah

> phew = Papu (Rocks "chases") (Yeah True)

2. typechecks

> truth = Papu (Rocks "chomskydoz") (Yeah True)

3. typechecks

> equalityForall :: Papu -> Papu -> Bool
> equalityForall p p' = p == p'

4. Does not type check because Ord is not instantiated

Chapter Exercises.lhs:106:25: error:
    • No instance for (Ord Papu) arising from a use of ‘>’
    • In the expression: p > p'
      In an equation for ‘equalityForall’: equalityForall p p' = p > p'
    |
106 | > equalityForall p p' = p > p'
    |                         ^^^^^^

Match the types

1. Replacement will NOT typecheck because i returns a number but the type
   cannot be known by i without a typeclass or concrete type in type definition

   i :: a
   i = 1

   Chapter Exercises.lhs:123:7: error:
       • No instance for (Num a) arising from the literal ‘1’
         Possible fix:
           add (Num a) to the context of
             the type signature for:
               i :: forall a. a
       • In the expression: 1
         In an equation for ‘i’: i = 1
       |
   123 | > i = 1
       |


   λ let i = 1
   λ :i i
   i :: Num p => p         -- Defined at <interactive>:1:5

2. Will NOT typecheck because 1.0 cannot be understood by Num typeclass.

   f :: Num a => a
   f = 1.0

   Chapter Exercises.lhs:141:7: error:
       • Could not deduce (Fractional a) arising from the literal ‘1.0’
         from the context: Num a
           bound by the type signature for:
                      f :: forall a. Num a => a
           at Chapter Exercises.lhs:140:3-17
         Possible fix:
           add (Fractional a) to the context of
             the type signature for:
               f :: forall a. Num a => a
       • In the expression: 1.0
         In an equation for ‘f’: f = 1.0
       |
   141 | > f = 1.0
       |       ^^^

   λ let f = 1.0
   λ :i f
   f :: Fractional p => p  -- Defined at <interactive>:3:5

3. Will typecheck because 1.0 is understood by the Fractional typeclass.

   λ let f = 1.0
   λ :i f
   f :: Fractional p => p  -- Defined at <interactive>:1:5

4. Will typecheck because 1.0 is understood by the RealFrac typeclass.

   λ let f = 1.0
   λ :i f
   f :: Fractional p => p  -- Defined at <interactive>:3:5

5. Will typecheck because adding a Ord typeclass won't change the `id` function

   λ let freud x = x
   λ :i freud
   freud :: p -> p         -- Defined at <interactive>:1:5

6. Will typecheck because adding a concert type is OK for restricting a function which uses the first parameter for the return

   λ let freud x = x
   λ :i freud
   freud :: p -> p         -- Defined at <interactive>:1:5

7. Will NOT typecheck because 'a' could be something other than an Int

> myX = 1 :: Int

   sigmund :: Int -> Int
   sigmund x = myX

   λ let myX = 1
   λ :i myX
   myX :: Num p => p       -- Defined at <interactive>:11:5

   λ :i sigmund
   sigmund :: p -> Int     -- Defined at <interactive>:4:5

8. Will NOT typecheck because Num could be something other than an Int

   sigmund' :: Int -> Int
   sigmund' x = myX

   λ :i sigmund
   sigmund :: p -> Int     -- Defined at <interactive>:4:5

9. Will typecheck because sort can work on a list

   jung :: [Int] -> Int
   jung xs = head (Data.List.sort xs)

   λ :i jung
   jung :: Ord a => [a] -> a       -- Defined at <interactive>:2:5

10. Will typecheck because it is the same type signature as 'jung' and we already know that works

    young :: Ord a => [a] -> a
    young xs = head (sort xs)

    λ :i young
    young :: Ord a => [a] -> a      -- Defined at <interactive>:1:5

11. Will NOT typecheck mySort is specific to 'Char' concrete type can cannot work for any 'Ord'

    > mySort :: [Char] -> [Char]
    > mySort = sort

    > signifier :: [Char] -> Char
    > signifier xs = head (mySort xs)

    Chapter Exercises.lhs:225:31: error:
        • Couldn't match type ‘a’ with ‘Char’
          ‘a’ is a rigid type variable bound by
            the type signature for:
              signifier :: forall a. Ord a => [a] -> a
            at Chapter Exercises.lhs:224:3-32
          Expected type: [Char]
            Actual type: [a]
        • In the first argument of ‘mySort’, namely ‘xs’
          In the first argument of ‘head’, namely ‘(mySort xs)’
          In the expression: head (mySort xs)
        • Relevant bindings include
            xs :: [a] (bound at Chapter Exercises.lhs:225:13)
            signifier :: [a] -> a (bound at Chapter Exercises.lhs:225:3)
        |
    225 | > signifier xs = head (mySort xs)
        |                               ^^

    λ :i signifier
    signifier :: [Char] -> Char     -- Defined at <interactive>:3:5

Type-Kwon-Do Two: Electric Typealoo

1.

> chk :: Eq b => (a -> b) -> a -> b -> Bool

One possible solution is to see if the result of the function equals the 'b' passed in as the 3rd parameter

> chk f a b = (f a) == b

2.

> arith :: Num b => (a -> b) -> Integer -> a -> b

One possiblility is to add 'i' to the result of the applied function '(a -> b) a'. Just need to convert
'Integer' to 'Num' with 'fromInteger'

> arith f i a = (f a) + (fromInteger i)
