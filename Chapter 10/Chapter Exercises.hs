stops   :: String
stops   = "pbtdkg"
vowels  :: String
vowels  = "aeiou"

-- 1.a)
allCombos :: String -> String -> [String]
allCombos stops' vowels' = [ x:y:z:[] | x <- stops', y <- vowels', z <- stops']

-- 1.b)
allCombos' :: String -> String -> [String]
allCombos' stops' vowels' = [ x:y:z:[] | x <- stops', y <- vowels', z <- stops', x == 'p']

-- 1.c)
nouns :: [String]
nouns = ["apple", "bandaid", "table"]
verbs :: [String]
verbs = ["eat", "put", "play"]

makeCombos :: [a] -> [b] -> [(a,b,a)]
makeCombos as bs = [(a, b, a') | a <- as, b <- bs, a' <- as]

{-- 2.

seekritFunc Gets the length of all the words in a string and sums them up.
It then divides the the sum by the total amount of words in the string
returning the average length of all of the words as a rounded down Int.
--}

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

-- 3

seekritFunc' :: String -> Double
seekritFunc' x = (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))

-- Rewriting functions using folds

-- 1.

myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.

myAny :: (a -> Bool) -> [a] -> Bool
myAny = (flip foldr) False . ((const .) . id)

-- 3

myElem :: Eq a => a -> [a] -> Bool
myElem = (.) ((flip foldr) False) ((const .) . (==))
