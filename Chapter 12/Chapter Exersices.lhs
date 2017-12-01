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

It's only Natural

> -- As natural as any competitive bodybuilder
> data Nat = Zero | Succ Nat deriving (Eq, Show)

> -- >>> integerToNat 0
> -- Just Zero
> -- >>> integerToNat 1
> -- Just (SuccZero)
> -- >>> integerToNat 2
> -- Just (Succ (Succ Zero))
> -- >>> integerToNat (-1)
> -- Nothing
> integerToNat :: Integer -> Maybe Nat
> integerToNat i
>   | i >= 0    = Just $ integerToNat' i
>   | otherwise = Nothing
>   where
>     integerToNat' :: Integer -> Nat
>     integerToNat' i'
>       | i' == 0    = Zero
>       | i' > 0     = Succ $ integerToNat' (i'-1)
>       | otherwise = undefined

Small library for Maybe

1.

> -- >>> isJust (Just 1)
> -- True
> -- >>> isJust Nothing
> -- False
> isJust :: Maybe a -> Bool
> isJust (Just _) = True
> isJust _ = False

> -- >>> isNothing (Just1)
> -- False
> -- >>> isNothing Nothing
> -- True
> isNothing :: Maybe a -> Bool
> isNothing Nothing = True
> isNothing _ = False

2.

> -- >>> mayybee 0 (+1) Nothing
> -- 0
> -- >>> mayybee 0 (+1) (Just 1)
> -- 2
> mayybee :: b -> (a -> b) -> Maybe a -> b
> mayybee _ f (Just a) = f a
> mayybee b _ Nothing = b

3.

> -- >>> fromMaybe 0 Nothing
> -- 0
> -- >>> fromMaybe 0 (Just 1)
> -- 1
> fromMaybe :: a -> Maybe a -> a
> fromMaybe a Nothing = a
> fromMaybe _ (Just a)= a

4.

> -- >>> listToMaybe [1,2,3]
> -- Just 1
> -- >>> listToMaybe []
> -- Nothing
> listToMaybe :: [a] -> Maybe a
> listToMaybe []    = Nothing
> listToMaybe (a:_) = Just a

5.

> -- >>> catMaybes [Just 1, Nothing, Just 2]
> -- [1,2]
> -- >>> let xs = take 3 $ repeat Nothing
> -- >>> catMaybes xs
> -- Nothing
> catMaybes :: [Maybe a] -> [a]
> catMaybes = flip foldr []
>               (\m acc ->
>                 case m of
>                 (Just x) -> x:acc
>                 Nothing  -> acc)

6.

> -- >>> flipMaybe [Just 1, Just 2, Just 3]
> -- Just [1,2,3]
> -- >>> flipMaybe [Just 1, Nothing, Just 3]
> -- Nothing
> flipMaybe :: [Maybe a] -> Maybe [a]
> flipMaybe []          = Just []
> flipMaybe (Nothing:_) = Nothing
> flipMaybe (Just x:xs) =
>   case flipMaybe xs of
>     Nothing -> Nothing
>     Just ys -> Just (x:ys)

Cheated on question 6 a little bit with some help from this answer:
https://github.com/dwayne/haskell-programming/blob/master/ch12/Maybe.hs

On my own I was searching for Nothing in the list and if that didn't exist then
I would create the list as normal without needing to check much. My solution was
not lazy.

> --  if (isNothingInList ml)
> --    then Nothing
> --    else Just $ catMaybes ml
> --  where
> --    isNothingInList []          = False
> --    isNothingInList (Nothing:_) = True
> --    isNothingInList (_:xs)      = isNothingInList xs
