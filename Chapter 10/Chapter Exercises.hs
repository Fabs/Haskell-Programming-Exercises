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
myAny = (flip foldr) False . ((||) .)

-- 3.

-- The below pointfree work was learned from this question on Stack Overflow:
-- https://stackoverflow.com/a/29596461/350221

myElem :: Eq a => a -> [a] -> Bool
--myElem e = foldr (\x acc -> acc || (e == x)) False
--myElem e = flip foldr False (\x acc -> acc || (e == x))
--myElem e = flip foldr False (\x acc -> (||) (e == x) acc)
--myElem e = flip foldr False (\x -> (||) (e == x))
--myElem e = flip foldr False (\x -> (||) ((==) e x))
--myElem e = flip foldr False ((||) . (\x -> (==) e x))
--myElem e = flip foldr False ((||) . ((==) e))
--myElem = \e -> flip foldr False ((||) . ((==) e))
--myElem = \e -> (flip foldr False) ((||) . ((==) e))
--myElem = (flip foldr False) . (\e -> (||) . ((==) e))
--myElem = (flip foldr False) . (\e -> ((.) (||)) ((==) e))
myElem = (flip foldr False) . (((.) (||)) . (==))
-- http://pointfree.io/ gives
--myElem = flip foldr False . (flip (||) .) . (==)

-- 4.

myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

-- 5.

myMap :: (a -> b) -> [a] -> [b]
--myMap f = (foldr (\x acc -> (f x):acc)) []
--myMap = flip (\f -> foldr (\x acc -> (f x):acc)) []
--myMap = flip (\f -> foldr (\x acc -> (:) (f x) acc)) []
--myMap = flip (\f -> foldr (\x -> ((:) (f x)))) []
--myMap = flip (\f -> foldr ((:) . (\x -> f x))) []
--myMap = flip (\f -> foldr ((:) . (f))) []
--myMap = flip (\f -> foldr ((:) . (f))) []
--myMap = flip (\f -> (foldr) ((.) (:) (f))) []
--myMap = flip ((foldr) . (\f -> (((.) (:)) (f)))) []
myMap = flip (foldr . ((.) (:))) []

-- 6.

myFilter :: (a -> Bool) -> [a] -> [a]
--myFilter f l = foldr (\x acc -> if f x then x:acc else acc) [] l
--myFilter f = foldr (\x acc -> if f x then x:acc else acc) []
--myFilter = \f -> foldr (\x acc -> if f x then x:acc else acc) []
--myFilter = flip (\f -> foldr (\x acc -> if f x then x:acc else acc)) []
--myFilter = flip (\f -> foldr (\x acc -> if' (f x) (x:acc) acc)) []
--myFilter = flip (\f -> foldr (\x acc -> (if' (f x) (x:acc)) acc)) []
--myFilter = flip (\f -> foldr (\x -> (\acc -> if' (f x) (x:acc)) <*> \acc -> acc)) []
--myFilter = flip (\f -> foldr (\x -> ((if' (f x)) . (\acc -> x:acc)) <*> id)) []
--myFilter = flip (\f -> foldr (\x -> ((if' (f x)) . ((x:) . id)) <*> id)) []
--myFilter = flip (\f -> foldr (flip (\x -> (<*>) ((if' (f x)) . ((x:) . id))) id)) []
--myFilter = flip (\f -> foldr (flip ((<*>) . (\x -> (if' (f x)) . ((x:) . id))) id)) []
--myFilter = flip (\f -> foldr (flip ((<*>) . (\x -> ((.) (if' (f x))) ((x:) . id))) id)) []
--myFilter = flip (\f -> foldr (flip ((<*>) . (((.) . (\x -> if' (f x))) <*> (\x -> (.) ((:) x) id))) id)) []
--myFilter = flip (\f -> foldr (flip ((<*>) . (((.) . ((if') . (\x -> f x))) <*> (flip (\x -> (.) ((:) x)) id))) id)) []
--myFilter = flip (\f -> foldr (flip ((<*>) . (((.) . (if' . f)) <*> (flip ((.) . (\x -> (:) x)) id))) id)) []
--myFilter = flip (foldr . (\f -> flip ((<*>) . (((.) . (if' . f)) <*> (flip ((.) . (:)) id))) id)) []
--myFilter = flip (foldr . (flip (\f -> flip ((<*>) . (((.) . (if' . f)) <*> (flip ((.) . (:)) id)))) id)) []
--myFilter = flip (foldr . (flip (flip . (\f -> (<*>) . (((.) . (if' . f)) <*> (flip ((.) . (:)) id)))) id)) []
--myFilter = flip (foldr . (flip (flip . (((<*>) .) . (\f -> ((.) . (if' . f)) <*> (flip ((.) . (:)) id)))) id)) []
--myFilter = flip (foldr . (flip (flip . (((<*>) .) . (\f -> ((<*>) ((.) . (if' . f))) (flip ((.) . (:)) id)))) id)) []
--myFilter = flip (foldr . (flip (flip . (((<*>) .) . (flip (\f -> (<*>) ((.) . (if' . f))) (flip ((.) . (:)) id)))) id)) []
--myFilter = flip (foldr . (flip (flip . (((<*>) .) . (flip ((<*>) . (\f -> (.) . (if' . f))) (flip ((.) . (:)) id)))) id)) []
--myFilter = flip (foldr . (flip (flip . (((<*>) .) . (flip ((<*>) . (((.) .) . (\f -> if' . f))) (flip ((.) . (:)) id)))) id)) []
myFilter = flip (foldr . (flip (flip . (((<*>) .) . (flip ((<*>) . (((.) .) . (if' .))) (flip ((.) . (:)) id)))) id)) []
  where
    if' :: Bool -> a -> a -> a
    --if' True x _ = x
    if' True = const
    --if' False _ y = y
    if' False = const id

-- 7.

squish :: [[a]] -> [a]
squish = foldr (\x acc -> x ++ acc) []

-- 8.

squishMap :: (a -> [b]) -> [a] -> [b]
--squishMap f = foldr (\x acc -> (f x) ++ acc) []
--squishMap = flip (\f -> foldr (\x acc -> (f x) ++ acc)) []
--squishMap = flip ((foldr) . (\f x acc -> (f x) ++ acc)) []
--squishMap = flip ((foldr) . (\f x -> (++) (f x))) []
--squishMap = flip ((foldr) . (\f -> (++) . (\x -> f x))) []
--squishMap = flip ((foldr) . (\f -> (++) . f)) []
squishMap = flip ((foldr) . ((.) (++))) []

-- 9.

squishAgain :: [[a]] -> [a]
--squishAgain l = squishMap (\x -> id x) l
squishAgain = squishMap id

-- 10.

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Gotta have something to order"
-- reverse required to meet second test example
-- Unable to make point free because of pattern matching above
myMaximumBy f (x:xs) = foldr g x (reverse xs)
  where
    --g = \b acc -> if' ((f acc b) == GT) acc b
    --g = \b -> flip (\acc -> if' ((f acc b) == GT) acc) b
    --g = \b -> flip ((\acc -> (if' ((f acc b) == GT))) <*> (\acc -> acc)) b
    --g = \b -> flip ((if' . (\acc -> (f acc b) == GT)) <*> id) b
    --g = \b -> flip ((if' . (flip (\acc -> (==) (f acc b)) GT)) <*> id) b
    --g = \b -> flip ((if' . (flip ((==) . (\acc -> f acc b)) GT)) <*> id) b
    --g = \b -> flip ((if' . (flip ((==) . (flip (\acc -> f acc) b)) GT)) <*> id) b
    --g = (flip . (\b -> (if' . (flip ((==) . (flip f b)) GT)) <*> id)) <*> id
    --g = (flip . (\b -> ((<*>) (if' . (flip ((==) . (flip f b)) GT))) id)) <*> id
    --g = (flip . (flip (\b -> (<*>) (if' . (flip ((==) . (flip f b)) GT))) id)) <*> id
    --g = (flip . (flip ((<*>) . (\b -> if' . (flip ((==) . (flip f b)) GT))) id)) <*> id
    --g = (flip . (flip ((<*>) . ((if' .) . (\b -> flip ((==) . (flip f b)) GT))) id)) <*> id
    --g = (flip . (flip ((<*>) . ((if' .) . (flip (\b -> flip ((==) . (flip f b))) GT))) id)) <*> id
    --g = (flip . (flip ((<*>) . ((if' .) . (flip (flip . (\b -> (==) . (flip f b))) GT))) id)) <*> id
    g = (flip . (flip ((<*>) . ((if' .) . (flip (flip . (((==) .) . (flip f))) GT))) id)) <*> id
    if' True = const
    if' False = const id

-- 11.

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []     = error "Gotta have something to order"
myMinimumBy f (x:xs) = foldr g x (reverse xs)
  where
    -- From above
    g = (flip . (flip ((<*>) . ((if' .) . (flip (flip . (((==) .) . (flip f))) LT))) id)) <*> id
    if' True = const
    if' False = const id
