Guard Duty

1.

> avgGrade_1 :: (Fractional a, Ord a) => a -> Char
> avgGrade_1 x
>   | otherwise = 'F'
>   | y >= 0.9  = 'A'
>   | y >= 0.8  = 'B'
>   | y >= 0.7  = 'C'
>   | y >= 0.59 = 'D'
>   | y < 0.59  = 'F'
>   where y = x / 100

λ avgGrade_1 90
'F'
λ avgGrade_1 75
'F'
λ avgGrade_1 60
'F'

2.

> avgGrade_2 :: (Fractional a, Ord a) => a -> Char
> avgGrade_2 x
>   | y >= 0.7  = 'C'
>   | y >= 0.9  = 'A'
>   | y >= 0.8  = 'B'
>   | y >= 0.59 = 'D'
>   | y < 0.59  = 'F'
>   where y = x / 100

λ avgGrade_2 90
'C'

3. b

> pal xs
>   | xs == reverse xs = True
>   | otherwise       = False

4. A list of Eq types.
5. pal :: Eq a => [a] -> Bool
6. c

> numbers x
>   | x < 0   = -1
>   | x == 0  = 0
>   | x > 0   = 1

7. The argument must be an Ord and Num.
8. numbers :: (Ord a, Num a, Num p) => a -> p
