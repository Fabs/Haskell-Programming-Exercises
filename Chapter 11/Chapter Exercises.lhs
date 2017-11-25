> import Data.Char
> -- import Data.List hiding (isSubsequenceOf)

Chapter Exercises

Multiple choice

1.

> data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday

a) Weekday IS a type with five data constructors

2. 

> f :: Weekday -> String
> f Friday = "Miller Time"
> f _ = undefined

c) type is f :: Weekday -> String

3. b) Types defined with the data keyword must begin with a capital letter 

4. 

> g :: [a] -> a
> g xs = xs !! (length xs - 1)

d) The function delivers the final element of xs

Ciphers

See Cipher.hs in chapter 9

As-patterns

1.

> isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
> isSubsequenceOf [] _              = True
> isSubsequenceOf _ []              = False
> isSubsequenceOf sub@(s:ss) full@(f':fs)
>   | s == f'   = isSubsequenceOf ss full
>   | otherwise = isSubsequenceOf sub fs

2.

> capitalizeWords :: String -> [(String, String)]
> capitalizeWords = map capitalizeWord' . words
>   where
>     capitalizeWord' :: String -> (String, String)
>     capitalizeWord' []          = ([],[])
>     capitalizeWord' word@(w:ws) = (word, (toUpper w):ws)
