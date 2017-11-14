Exercises: Filtering

1.

> q1 xs = [ x | x <- xs, rem x 3 == 0 ]

2.

> q2 = (length . q1)

3.

> myFilter :: String -> [String]
> myFilter = (filter (not . flip elem ["the", "a", "an"]) . words)
