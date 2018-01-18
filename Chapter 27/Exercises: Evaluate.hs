-- 1.

main :: IO ()
main = do
  putStr "const 1 undefined == 1 -- "
  print $ const 1 undefined == (1::Int)

  putStrLn "const undefined 1 == 1 -- bottom"
  -- print $ const undefined (1::Int) == 1

  putStr "flip const undefined 1 == 1 -- "
  print $ flip const undefined 1 == (1::Int)

  putStrLn "flip const 1 undefined == 1 -- bottom"
  --print $ flip const (1::Int) undefined == (1::Int)

  putStr "const undefined undefined == 1 -- bottom"
  --print $ const undefined undefined == (1::Int)

  putStr "foldr const 'z' ['a'..'e'] == 'a' -- "
  print $ foldr const 'z' ['a'..'e'] == 'a'

  putStr "foldr (flip const) 'z' ['a'..'e'] == 'z' -- "
  print $ foldr (flip const) 'z' ['a'..'e'] == 'z'
