Exercises: For Example

> data Example = MakeExample Int deriving Show

1. 

λ :t MakeExample
MakeExample :: Example
λ :t Example

<interactive>:1:1: error: Data constructor not in scope: Example

2.

λ :info Example
data Example = MakeExample
        -- Defined at Exercises: For Example.lhs:3:3
instance [safe] Show Example
  -- Defined at Exercises: For Example.lhs:3:39

Show is defined for Example by looking at info

3.

λ :t MakeExample
MakeExample :: Int -> Example

The number of arugments change for the type
