Chapter Exercises

Determine the kinds

1. id :: a -> a

the kind of a is *

2. r :: a -> fa

The kind of a is *
The kind of f is * -> *

String Processing

1.

> notThe :: String -> Maybe String
> notThe "the" = Nothing
> notThe s     = Just s

Did not need to use the notThe helper but just some tricky pattern matching

> replaceThe :: String -> String
> replaceThe ('t':'h':'e':' ':cs) = replaceThe $ 'a':' ':cs
> replaceThe s                    = replaceThe' s
>   where
>     replaceThe' []     = []
>     replaceThe' (' ':'t':'h':'e':' ':cs) = replaceThe' $ ' ':'a':' ':cs
>     replaceThe' (' ':'t':'h':'e':[]) = ' ':'a':[]
>     replaceThe' (c:cs)         = c:(replaceThe' cs)

2.

Use the same matching tricks as replaceThe

> countTheBeforeVowel :: String -> Integer
> countTheBeforeVowel ('t':'h':'e':' ':v:cs)
>   | elem v "aeiou" = 1 + countTheBeforeVowel (v:cs)
>   | otherwise      = countTheBeforeVowel (v:cs)
> countTheBeforeVowel s = countTheBeforeVowel' s
>   where
>     countTheBeforeVowel' :: String -> Integer
>     countTheBeforeVowel' [] = 0
>     countTheBeforeVowel' (' ':'t':'h':'e':' ':v:cs)
>       | elem v "aeiou" = 1 + countTheBeforeVowel' (v:cs)
>       | otherwise      = countTheBeforeVowel' (v:cs)
>     countTheBeforeVowel' (_:cs)         = countTheBeforeVowel' cs

3.

> countVowels :: String -> Integer
> countVowels []     = 0
> countVowels (c:cs)
>   | elem c "aeiou" = 1 + countVowels cs
>   | otherwise      = countVowels cs

Validate the word

> newtype Word' = Word' String deriving (Eq, Show)

> vowels :: String
> vowels = "aeiou"

> mkWord :: String -> Maybe Word'
> mkWord w = case vowelCount > consonantCount of
>   True  -> Nothing
>   False -> Just $ Word' w
>   where
>     vowelCount = countVowels w
>     consonantCount = foldr (\x acc -> if elem x consonants then acc+1 else acc) 0 w
>     consonants = [ x  | x <- ['a'..'z'], not $ elem x vowels ]
