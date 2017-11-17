Chapter Exercises

> import Data.Char

Data.Char

1. λ :t isUpper
   isUpper :: Char -> Bool


   λ :t toUpper
   toUpper :: Char -> Char

2.

> q2 :: [Char] -> [Char]
> q2 = filter isUpper

3.

> q3 :: [Char] -> [Char]
> q3 []     = []
> q3 (x:xs) = (toUpper x):xs

4.

> q4 :: [Char] -> [Char]
> q4 ('w':'o':'o':'t':[]) = "WOOT"
> q4 x = q3 x

5.
6.

> capHead :: String -> Char
> -- capHead ([]) = error "Sorry! Empty string is not allowed."
> capHead = (toUpper . head)

Ciphers

See Chipher.hs

Writing your own standard functions

1.

> myOr :: [Bool] -> Bool
> myOr []          = False
> myOr (True:_)    = True
> myOr (False:xs)  = myOr xs

2.

> myAny :: (a -> Bool) -> [a] -> Bool
> myAny _ []      = False
> myAny f (x:xs)
>   | f x       = True
>   | otherwise = myAny f xs

3.

> myElem :: Eq a => a -> [a] -> Bool
> myElem _ []     = False
> myElem a (x:xs)
>   | a == x      = True
>   | otherwise   = myElem a xs

4.

> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse (x:xs) = (myReverse xs) ++ [x]

5.

> squish :: [[a]] -> [a]
> squish []     = []
> squish (x:xs) = x ++ (squish xs)

6.

> squishMap :: (a -> [b]) -> [a] -> [b]
> squishMap _ []     = []
> squishMap f (x:xs) = (f x) ++ (squishMap f xs)

7.

> squishAgain :: [[a]] -> [a]
> squishAgain = squishMap id

8.

> myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
> myMaximumBy _ []        = error "Gotta give me something!"
> myMaximumBy _ (x:[])    = x
> myMaximumBy f (x:xs)
>   | f x y == GT         = x
>   | otherwise           = y
>   where y = myMaximumBy f xs

9.

> myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
> myMinimumBy _ []        = error "Gotta give me something!"
> myMinimumBy _ (x:[])    = x
> myMinimumBy f (x:xs)
>   | f x y == LT         = x
>   | otherwise           = y
>   where y = myMaximumBy f xs

10.

> myMaximum :: (Ord a) => [a] -> a
> myMaximum = myMaximumBy compare

> myMinimum :: (Ord a) => [a] -> a
> myMinimum = myMinimumBy compare
