> awesome = ["Papuchon", "curry", ":)"]
> alsoAwesome = ["Quake", "The Simons"]
> allAwesome = [awesome, alsoAwesome]

1. The type signature is:

```
length :: [a] -> Integer
```

One argument which is a list of anything.

The return value is an `Integer`.

From GHCI:

```
Prelude> :t length
length :: Foldable t => t a -> Int
```

2.
a) 5
```
Prelude> length [1,2,3,4,5]
5
```

b) 3
```
Prelude> length [(1,2), (2,3), (3,4)]
3
```

c) 2
```
*Main> length allAwesome
2
```

d) 5
```
*Main> length (concat allAwesome)
5
```

3. `6 / length [1, 2, 3]` returns an error because `/` should only be applied to two `Fractional` arguments but there is one `Int` argument.

4. A ``div`` can be used to do an interger division. Like this `6 `div` length [1,2,3]`.

5. A `Bool` expression. The result should be `True`.
```
Prelude> 2 + 3 == 5
True
```

6. `False`.
```
Prelude> let x = 5
Prelude> x + 3 == 5
False
```

7.

`length allAwesome == 2` should be `True`.
```
*Main> length allAwesome == 2
True
```

`length [1, 'a', 3, 'b']` should be an error.
```
*Main> length [1, 'a', 3, 'b']

<interactive>:2:9: error:
    • No instance for (Num Char) arising from the literal ‘1’
    • In the expression: 1
      In the first argument of ‘length’, namely ‘[1, 'a', 3, 'b']’
      In the expression: length [1, 'a', 3, 'b']
```

`length allAwesome + length awesome` should be `5`.
```
*Main> length allAwesome + length awesome
5
```

`(8 == 8) && ('b' < 'a')` should be `False`.
```
*Main> (8 == 8) && ('b' < 'a')
False
```
`(8 == 8) && 9` should be an error.
```
*Main> (8 == 8) && 9

<interactive>:5:13: error:
    • No instance for (Num Bool) arising from the literal ‘9’
    • In the second argument of ‘(&&)’, namely ‘9’
      In the expression: (8 == 8) && 9
      In an equation for ‘it’: it = (8 == 8) && 9
```

8.

> isPalindrome :: (Eq a) => [a] -> Bool
> isPalindrome x = reverse x == x

9.

> myAbs :: Integer -> Integer
> myAbs x = if x < 0
>           then x * (-1)
>           else x

10.

> f :: (a, b) -> (c, d) -> ((b, d), (a, c))
> f x y = (((snd x), (snd y)), ((fst x), (fst y)))

Correcting Syntax

1.

> x = (+)
> aF xs = w `x` 1 
>      where w = length xs

2.

> aX x = x

3.

> aa (x:_) = x

4.

> f' (a,b) = a

Match the function names to their types

1. a
2. b
3. a
4. d
