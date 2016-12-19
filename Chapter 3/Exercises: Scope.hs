{--
1. Yes

Prelude> let x=5
Prelude> let y=7
Prelude> let z = x * y
Prelude> z
35

2. No

Prelude> let f=3
Prelude> let g=6*f+h

<interactive>:11:11: error: Variable not in scope: h

3. No. Missing a definition for `d`.

> ghci "Exercises: Scope - 3.hs"
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( Exercises: Scope - 3.hs, interpreted )

Exercises: Scope - 3.hs:2:5: error: Variable not in scope: d
Failed, modules loaded: none.

4. No. Still missing a defition for `d`.

> ghci "Exercises: Scope - 4.hs"
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( Exercises: Scope - 4.hs, interpreted )

Exercises: Scope - 4.hs:2:5: error: Variable not in scope: d
Failed, modules loaded: none.
--}

