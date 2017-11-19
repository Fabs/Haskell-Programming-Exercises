Exercises: Understanding Folds

1. b and c

2.

foldl (flip (*)) 1 [1..3]

foldl (flip (*)) (1 * 1) [2,3]
foldl (flip (*)) (2 * (1 * 1)) [3]
(3 * (2 * (1 * 1)))
6

λ foldl (flip (\x y -> concat ["(",x,"*",y,")"])) "1" (map show [1..3])
"(3*(2*(1*1)))"

3. c
Right folds associate to the right

4. a

5.
a) foldr (++) [] ["woot", "WOOT", "woot"]
b) foldr max ' ' "fear is the little death"
c) foldr (&&) True [False, True]
d) It will never return a different answer than True unless the initial value is
False.

foldr (||) False [False, True]

e) foldl (flip ((++) . show)) "" [1..5]
f) foldr const 'a' ['1'..'5'] or foldr (flip const) 'a' [1..5]
g) foldr (flip const) 0 "tacos"
h) foldl const 0 "burritos"
i) foldl const 'z' [1..5]
