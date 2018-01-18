What will :sprint output

1. let x = 1
x = _

2. let x = ['1']
x = "1"

3. let x = [1]
x = _

4. x = 1 :: Int
x = 1

5. let f = \x -> x
   let x = f 1
f = _
x = _

6. let f :: Int -> Int; f = \x -> x
   let x = f 1
f = _
x = _

Will printing this expression result in bottom?

1. snd (undefined, 1)
No

2. let x = undefined
   let y = x `seq` 1 in snd (x, y)
Yes, because of `seq` forcing x

3. length $ [1..5] ++ undefined
Yes, because undefined is not something that can be folded. ++ [undefined] would work.

4. length $ [1..5] ++ [undefined]
No

5. const 1 undefined
No

6. const 1 (undefined `seq` 1)
No

7. const undefined 1
Yes

Make the expression bottom

1.

> x = undefined
> y = "blah"
> main = do
>   print (snd (x, seq x y))
