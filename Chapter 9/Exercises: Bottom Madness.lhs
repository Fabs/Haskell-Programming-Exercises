Exercises: Bottom Madness

1. ⊥

   λ [x^y | x <- [1..5], y <- [2, undefined]]
   [1,*** Exception: Prelude.undefined
   CallStack (from HasCallStack):
     error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
     undefined, called at <interactive>:7:30 in interactive:Ghci4

2. return a value

   λ take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
   [1]

3. ⊥
4. return a value

   λ length [1, 2, undefined]
   3

5. ⊥
6. return a value because `filter even` will only evaluate the first 2 elements and not undefined

   λ filter even [1,2,3, undefined]
   [2*** Exception: Prelude.undefined
   CallStack (from HasCallStack):
     error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
     undefined, called at <interactive>:14:21 in interactive:Ghci7

7. ⊥
8. return a value for a similar reason as in question 6
9. return a value
10. ⊥

Intermission: Is it in normal form?

1. NF
2. WHNF
3. neither
4. neither
5. neither
6. neither
7. WHNF
