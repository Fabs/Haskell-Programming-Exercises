Chapter Exercises

Review of types

1. d
2. b
3. d
4. b 

Review currying

> cattyConny :: String -> String -> String
> cattyConny x y = x ++ " mrow " ++ y

> flippy :: String -> String -> String
> flippy = flip cattyConny

> appedCatty :: String -> String
> appedCatty = cattyConny "woops"

> frappe :: String -> String
> frappe = flippy "haha"

1. "woops mrow woohoo!"
2. "1 mrow haha"
3. "woops mrow 2 mrow haha"
4. "woops mrow blue mrow haha"

5.

cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
cattyConny (frappe "pink") (cattyConny "green" "woops mrow blue")
cattyConny (frappe "pink") "green mrow woops mrow blue"
cattyConny "pink mrow haha" "green mrow woops mrow blue"
"pink mrow haha mrow green mrow woops mrow blue"

6.

cattyConny (flippy "Pugs" "are") "awesome"
cattyConny "are mrow Pugs" "awesome"
"are mrow Pugs mrow awesome"

Recursion

> dividedBy :: Integral a => a -> a -> (a, a)
> dividedBy num denom = go num denom 0
>   where go n  d count
>          | n < d = (count, n)
>          | otherwise = go (n-d) d (count + 1)

1.

dividedBy 15 2

go 13 2 1
go 11 2 2
go 9 2 3
go 7 2 4
go 5 2 5
go 3 2 6
go 1 2 7

(7, 1)

2.

> sumUp :: (Eq a, Num a) => a -> a
> sumUp 0 = 0
> sumUp n = n + sumUp (n -1) 

3.

> myMult :: (Integral a) => a -> a -> a
> myMult x 0 = 0
> myMult x y = x + myMult x (y-1)

Fixing dividedBy

> dividedBy' :: Integral a => a -> a -> (a, a)
> dividedBy' num denom = go num denom 0
>   where go n  d count
>          | (abs n) < (abs d) = (count, n)
>          | (n < 0 && d > 0) || (n > 0 && d < 0) = go (n+d) d (count - 1)
>          | otherwise = go (n-d) d (count + 1)
