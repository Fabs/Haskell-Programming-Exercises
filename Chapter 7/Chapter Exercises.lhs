Chapter Exercises for chapter 7

Multiple choice

1. d
2. b
3. d
4. b
5. a

Let's write code

1.

a.

> tensDigit :: Integral a => a -> a
> tensDigit x = d
>   where (xLast, _) = x `divMod` 10
>         (_, d)     = xLast `divMod` 10

b. Yes
c.

> hunsD :: Integral a => a -> a
> hunsD x = d
>   where (xLast, _) = x `divMod` 100
>         (_, d)     = xLast `divMod` 10

2.

> foldBool1 :: a -> a -> Bool -> a
> foldBool1 x y z = case z of
>   True -> y
>   False -> x

> foldBool2 :: a -> a -> Bool -> a
> foldBool2 x y z
>   | z == True = y
>   | z == False = x

> foldBool3 :: a -> a -> Bool -> a
> foldBool3 x _ False = x
> foldBool3 _ y True  = y

3.

> g :: (a -> b) -> (a, c) -> (b, c)
> g f (a, c) = (f a, c)

4. See `arith4.hs`

5. See `arith4.hs`

6. See `arith4.hs`
