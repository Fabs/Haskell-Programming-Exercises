Exercises: Pity the Bool

1. 4
2. Cardinality is 258

> import Data.Int
> data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)


λ let x = Numba (-129)

<interactive>:4:17: warning: [-Woverflowed-literals]
    Literal -129 is out of the Int8 range -128..127
