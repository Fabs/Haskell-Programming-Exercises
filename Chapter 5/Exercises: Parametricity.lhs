1.

I couldn't think of a way to make a `not_id` function.
I tried `not_id a = 1` but it didn't work.

> not_id :: a -> a
> not_id a = a

2.

> two_a :: a -> a -> a
> two_a x _ = x

> two_a' :: a -> a -> a
> two_a' _ x = x
