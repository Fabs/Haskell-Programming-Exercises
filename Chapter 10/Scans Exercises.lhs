Scans Exercises

1.

> myFibs1 :: [Integer]
> myFibs1 = take 20 $ 1 : scanl (+) 1 myFibs1

> myFibs2 :: [Integer]
> myFibs2 = takeWhile (< 100) $ 1 : scanl (+) 1 myFibs2

> myFactorial :: [Integer]
> myFactorial = 1 : scanl (*) 2 myFactorial
