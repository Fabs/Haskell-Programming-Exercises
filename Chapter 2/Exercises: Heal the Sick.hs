{--

1.

Prelude> let area x = 3. 14 * (x * x)

<interactive>:1:5: error:
    • Non type-variable argument in the constraint: Num (a -> c)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        area :: forall b c a.
                (Num (a -> c), Num (a -> b), Num (b -> c)) =>
                (a -> c) -> a -> c
Prelude> let area x = 3.14 * (x * x)


2.

Prelude> let double x = b * 2

<interactive>:3:16: error: Variable not in scope: b
Prelude> let double x = x * 2

--}

-- 3.

x = 7
y = 10
f = x + y
