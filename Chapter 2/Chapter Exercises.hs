{--

1. 2 + 2 * 3 -1

Prelude> 2 + 2 * 3 - 1
7
Prelude> 2 + (2 * 3) - 1
7

2.

Prelude> (^) 10 $ 1 + 1
100
Prelude> (1 + 1) ^ 10
1024
Prelude> 10 ^ (1 + 1)
100

3.

Prelude> 2 ^ 2 * 4 ^ 5 + 1
4097
Prelude> (2 ^ 2) * (4 ^ 5) + 1
4097
Prelude> ((2 ^ 2) * (4 ^ 5)) + 1
4097

Equivalent expressions

1. Same result

Prelude> 1 + 1
2

2. Same result

Prelude> 10 ^ 2
100
Prelude> 10 + 9 * 10
100

3. Different result

Prelude> 400 - 37
363
Prelude> (-) 37 400
-363

4. Different result

Prelude> 100 `div` 3
33
Prelude> 100 / 3
33.333333333333336

5. Different result

Prelude> 2 * 5 + 18
28
Prelude> 2 * (5 + 18)
46

More fun with functions

Prelude> z = 7
Prelude> x = y ^ 2

<interactive>:24:5: error: Variable not in scope: y
Prelude> y = z + 8
Prelude> x = y ^ 2
Prelude> waxOn = x * 5

Manual work here
z = 7
y = z + 8 = 15
x = y ^ 2 = 15 ^ 2 = 225
waxOn = x * 5 = 225 * 5 = 1125

1. 10 + waxOn = 1135

Prelude> 10 + waxOn
1135

(+10) waxOn = 1135

Prelude> (+10) waxOn
1135

(-) 15 waxOn = -1110

Prelude> (-) 15 waxOn
-1110

(-) waxOn 15 = 1110

Prelude> (-) waxOn 15
1110

2. and 3.

Prelude> triple x = x * 3
Prelude> triple waxOn
3375

--}

waxOn     = x * 5 
  where z = 7
        x = y ^ 2
        y = z + 8

triple x = x * 3

waxOff x = triple x

{--

*Main> waxOff 10
30
*Main>
*Main> waxOff (-50)
-150

--}
