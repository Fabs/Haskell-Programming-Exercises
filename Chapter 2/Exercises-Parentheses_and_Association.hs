-- 1. Yes
exercise1 = do
  putStrLn $ show $ 8 + 7 * 9
  putStrLn $ show $ (8 + 7) * 9

-- 2. No
exercise2 = do
  putStrLn $ show $ perimitera 4 2
  putStrLn $ show $ perimiterb 4 2
  where
    perimitera x y = (x * 2) + (y * 2)
    perimiterb x y = x * 2 + y * 2

-- 3. Yes
exercise3 = do
  putStrLn $ show $ fa 3
  putStrLn $ show $ fb 3
  where
    fa x = x / 2 + 9
    fb x = x / (2 + 9)
