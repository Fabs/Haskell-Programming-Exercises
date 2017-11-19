Scans Exercises

1.

> myFibs1 :: [Integer]
> myFibs1 = take 20 $ 1 : scanl (+) 1 myFibs1

> myFibs2 :: [Integer]
> myFibs2 = takeWhile (< 100) $ 1 : scanl (+) 1 myFibs2


factorial is not defined with itself but it is using scanl.
I couldn't figure out how to define a factorial in terms of itself with scanl.

> myFactorial :: [Integer]
> myFactorial = scanl (*) 1 [1..]
