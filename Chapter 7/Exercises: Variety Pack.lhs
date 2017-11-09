1. 

> k (x, y) = x
> k1 = k ((4-1), 10)
> k2 = k ("three", (1 + 2))
> k3 = k (3, True)

a) k :: (a, b) -> a

b) k2 :: [Char]

   k2 is not the same type as k1 or k3

c) k3

2.

> f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
> f (a, b, c) (d, e, f) = ((a, d), (c, f))
