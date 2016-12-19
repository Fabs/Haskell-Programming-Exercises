{--
1.
Prelude> ++ [1,2,3] [4,5,6]

<interactive>:1:1: error: parse error on input ‘++’
Prelude> (++) [1,2,3] [4,5,6]
[1,2,3,4,5,6]
Prelude> [1,2,3] ++ [4,5,6]
[1,2,3,4,5,6]

2.
Prelude> '<3' ++ ' Haskell'

<interactive>:1:2: error: parse error on input ‘<’
Prelude> "<3" ++ " Haskell"
"<3 Haskell"

3.
Prelude> concat ["<3", " Haskell"]
"<3 Haskell"
--}
