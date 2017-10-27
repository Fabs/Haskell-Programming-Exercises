λ :t quotRem
quotRem :: Integral a => a -> a -> (a, a)

λ :t divMod
divMod :: Integral a => a -> a -> (a, a)

It looks like they return a tuple of Integrals. I'm assuming the `quot` or `div` and then the `mod` or 'rem`.

λ divMod 100 43
(2,14)

λ quotRem 100 43
(2,14)
