1.

> myConcat x = x ++ " yo"

(++) :: [a] -> [a] -> [a]
It would change to
(++) :: [Char] -> [Char] -> [Char]

From GHCI:

```
λ :t myConcat                                  
myConcat :: [Char] -> [Char]
```

2.

> myMult x = (x / 3) * 5

```
λ :t myMult                                    
myMult :: Fractional a => a -> a
```

(*) :: Num a => a -> a -> a
It would change to
(*) :: Fractional a => a -> a -> a

3.

> myTake x = take x "hey you"

```
λ :t myTake
myTake :: Int -> [Char]
```

take :: Int -> [a] -> [a]
would change to
take :: Int -> [Char] -> [Char]


4.

> myCom x = x > (length [1..10])

```
λ :t myCom
myCom :: Int -> Bool
```

(>) :: Ord a => a -> a -> Bool
would change to
(>) :: Int -> Int -> Bool

5.

> myAlph x = x < 'z'

```
λ :t myAlph 
myAlph :: Char -> Bool
```

(<) :: Ord a => a -> a -> Bool
would change to
(<) :: Char -> Char -> Bool
