Multiple Choice

1. c
2. a
3. b
4. c

Determine the type

1. a) Num a => a
   b) Num a => (a, [Char])
   c) (Integer, [Char])
   d) Bool
   e) Int
   f) Bool

2. Num a => a

3. Num a => a -> a

4. Fractional a => a

5. [Char]

Does it compile?

1.  Doesn't compile

I'm not sure what wahoo is trying to do. Assuming 10 is an agrument for bigNum

> bigNum x = (^) 5 $ x
> wahoo = bigNum $ 10

2. Compiles

> x = print
> y = print "woohoo!"
> z = x "hello world"

3. Doesn't compile

I assume a is supposed to be used for 'addition'

> a = (+)
> b = 5
> c = a b 10
> d = a c 200

4. Doesn't compile

Missing 'c'. Prefixed function with '_4'.

> _4a = 12 + _4b
> _4b = 1000 * _4c
> _4c = 1

Type variable or specific type constructor?

1. f :: Num a => a -> b -> Int -> Int
                [0]  [1]   [2]    [3]

  0: constrained polymorphic
  1: fully polymorphic
  2 and 3: concrete

2. f :: zed -> Zed -> Blah
        [0]    [1]    [2]

  0: fully polymorphic
  1 and 2: concrete

3. f :: Enum b => a -> b -> C
                 [0]  [1]  [2]

  0: fully polymorphic
  1: constrained polymorphic
  2: concrete

4: f :: f -> g -> C
       [0]  [1]  [2]

  0 and 1: fully polymorphic
  2: concrete

Write a type signature

1. functionH :: [a] -> a

> functionH (x:_) = x

2. functionC :: Ord a => a -> a -> Bool

> functionC x y = if (x > y) then True else False

3. functionS :: (a, b) -> b

> functionS (x, y) = y

Given a type, write the function

1.

> i :: a -> a
> i x = x

2.

> _2c :: a -> b -> a
> _2c x _ = x

3.

> _2c' :: b -> a -> b
> _2c' x _ = x

4.

> c' :: a -> b -> b
> c' _ y = y

5. My implimentation returns the first element if it exists or else '[]'

> r :: [a] -> [a]
> r (x:_) = [x]
> r _ = []

6.

> co :: (b -> c) -> (a -> b) -> a -> c
> co bToC aToB a = (bToC . aToB) a

7.

> _7a :: (a -> c) -> a -> a
> _7a _ a = a

8.

> _8a' :: (a -> b) -> a -> b
> _8a' aToB a =  aToB a

Fix it

1. See `mySong` in `sing.hs`
2. See `mySong'`in `sing.hs`
3. See `arith3Broken.hs`

Type-Kwon-Do

See `typeKwonDo.hs`
