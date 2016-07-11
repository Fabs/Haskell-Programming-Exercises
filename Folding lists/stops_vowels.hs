stops   = "pbtdkg"
vowels  = "aeiou"

-- 1.a)
allPossibleCombos :: String -> String -> [(Char, Char, Char)]
allPossibleCombos stops vowels = allPossibleCombos' stopCombos vowels where
  stopCombos = allCombinationsOfTwo stops
  allPossibleCombos' :: [(Char,Char)] -> String -> [(Char, Char, Char)]
  allPossibleCombos' _ []           = []
  allPossibleCombos' combos (x:xs)  = insertCharIntoTupleList x combos ++ allPossibleCombos' combos xs

-- Finds all combinations of two Chars in a String
allCombinationsOfTwo :: String -> [(Char, Char)]
allCombinationsOfTwo ""     = []
allCombinationsOfTwo (x:xs) = allPairsOf x (x:xs) ++ allCombinationsOfTwo xs
  where
    allPairsOf :: Char -> String -> [(Char,Char)]
    allPairsOf _ []   = []
    allPairsOf c (x:xs) 
      | c == x    = (c,x):(allPairsOf c xs)
      | otherwise = (c,x):(x,c):(allPairsOf c xs)

insertCharIntoTupleList :: Char -> [(Char, Char)] -> [(Char, Char, Char)]
insertCharIntoTupleList c lst = map (\(x,y) -> (x,c,y)) lst

-- Exercise 1.b)

-- Looking at the solution here: https://github.com/lukleh/haskell-book-exercises/blob/gh-pages/ch10/ch10_10.10_0.hs
-- I realized it is much easier to use list comprehension!

allPossibleCombos' :: String -> String -> [(Char, Char, Char)] 
allPossibleCombos' stops vowels =
  [(s, v, s') | s <- stops, s == 'p', v <- vowels, s' <- stops]

-- Exercise 1.c)
nouns = ["apple", "bandaid", "table"]
verbs = ["eat", "put", "play"]

makeCombos :: [a] -> [b] -> [(a,b,a)]
makeCombos as bs = [(a, b, a') | a <- as, b <- bs, a' <- as]
