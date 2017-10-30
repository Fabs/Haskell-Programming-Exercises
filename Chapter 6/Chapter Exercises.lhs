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
