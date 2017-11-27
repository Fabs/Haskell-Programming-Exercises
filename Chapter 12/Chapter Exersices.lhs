Chapter Exercises

Determine the kinds

1. id :: a -> a

the kind of a is *

2. r :: a -> fa

The kind of a is *
The kind of f is * -> *

String Processing

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
