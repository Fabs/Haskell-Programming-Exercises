Exercises: More Bottoms

> import Data.Bool

1. ⊥
2. value
3. ⊥
4. Goes through all the characters in first argument and returns a list the
   boolean values which represent if the character is a member of the list "aeiou".

> itIsMystery :: [Char] -> [Bool]
> itIsMystery = map (\x -> elem x "aeiou")

5. a) [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
   b) [1, 10, 20]
   c) [15, 15, 15]

6.

> q6 = map (\x -> bool x (-x) (x == 3)) [1..10]
