{--
1.

Prelude> let x = 5 in x
5

2.

Prelude> let x = 5 in x * x
25

3.

Prelude> let x = 5; y = 6 in x * y
30

4.

Prelude> let x = 3; y = 1000 in x + 3
6
--}

exercise1 = x * 3 + y
  where x = 3
        y = 1000
    
exercise2 = x * 5
  where y = 10
        x = 10 * 5 + y

exercise3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10
