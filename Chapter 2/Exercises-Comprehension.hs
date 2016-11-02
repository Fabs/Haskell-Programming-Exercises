{-- Comprehension Check

1.

*Main> let half x = x/2
*Main> half 4
2.0
*Main> let square x = x * x
*Main> square 4
16

--}
exercise1 =
  let half x = x / 2
      square x = x * x
  in do
    putStrLn $ "half 4 " ++ (show $ half 4)
    putStrLn $ "square 4 " ++ (show $ square 4)

-- 2.
exercise2 x = 3.14 * (x * x)

-- 3.
exercise3 x = pi * (x * x)
