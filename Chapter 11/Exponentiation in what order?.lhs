Exponentiation in what order? 

> data Quantum =
>     Yes
>   | No
>   | Both
>   deriving (Eq, Show)

> convert1 :: Quantum -> Bool
> convert1 _ = True

> convert2 :: Quantum -> Bool
> convert2 Yes = True
> convert2 _   = False

> convert3 :: Quantum -> Bool
> convert3 No = True
> convert3 _  = False

> convert4 :: Quantum -> Bool
> convert4 Both = True
> convert4 _    = False

> convert5 :: Quantum -> Bool
> convert5 Yes = False
> convert5 _   = True

> convert6 :: Quantum -> Bool
> convert6 No = False
> convert6 _  = True

> convert7 :: Quantum -> Bool
> convert7 Both = False
> convert7 _    = True

> convert8 :: Quantum -> Bool
> convert8 _ = False

Yes, there are 2^3 possible combinations
