1.  All are equivalent.

> mTha x y z = x * y * z
> mThb x y = \z -> x * y * z
> mThc x = \y -> \z -> x * y * z

> mThd :: Num a => a -> a -> a -> a
> mThd = \x -> \y -> \z -> x * y * z

2. d

λ :t mThd 3
mThd 3 :: Num a => a -> a -> a

3. a)

> addOneIfOdd = \n -> case odd n of; True -> f n; False -> n; where f n = n + 1
> addFive = \x -> \y -> (if x > y then y else x) + 5
> mflip = \f -> \x -> \y -> f y x
