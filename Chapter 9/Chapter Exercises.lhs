Chapter Exercises

> import Data.Char

Data.Char

1. λ :t isUpper
   isUpper :: Char -> Bool


   λ :t toUpper
   toUpper :: Char -> Char

2.

> q2 = filter isUpper

3.

> q3 []     = []
> q3 (x:xs) = (toUpper x):xs

4.

> q4 ('w':'o':'o':'t':[]) = "WOOT"
> q4 x = q3 x
