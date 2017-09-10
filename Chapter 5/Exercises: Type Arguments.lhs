1. a
f x :: Char -> Char -> Char

2. d
g 0 'c' "woot" :: Char

3. b
h 1.0 2 :: Num b => b

4. c
h 1 (5.5 :: Double) :: Double

5. a
jackel "keyboard" "has the word jackel in it" :: [Char]

6. e
jackel "keyboard" :: Eq b => b -> [Char]

7. d
kessel 1 2 :: (Num a, Ord a) => a

8. e
kessel 1 (2 :: Integer) :: (Num a, Ord a) => a

9. c
kessel (1 :: Integer) 2 :: Integer
