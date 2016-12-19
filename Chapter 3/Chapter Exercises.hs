{--
Reading Syntax
1.

a) Yes

Prelude> concat [[1,2,3], [4,5,6]]
[1,2,3,4,5,6]

b) No

Prelude> ++ [1,2,3] [4,5,6]

<interactive>:2:1: error: parse error on input ‘++’
Prelude> (++) [1,2,3] [4,5,6]
[1,2,3,4,5,6]

c) Yes

Prelude> (++) "hello" " world"
"hello world"

d) No

Prelude> ["hello" ++ " world]

<interactive>:6:21: error:
    lexical error in string/character literal at end of input
Prelude> ["hello" ++ " world"]
["hello world"]

e) No

Prelude> 4 !! "hello"

<interactive>:8:6: error:
    • Couldn't match expected type ‘Int’ with actual type ‘[Char]’
    • In the second argument of ‘(!!)’, namely ‘"hello"’
      In the expression: 4 !! "hello"
      In an equation for ‘it’: it = 4 !! "hello"
Prelude> "hello" !! 4
'o'

f) Yes

Prelude> (!!) "hello" 4
'o'

g) No

Prelude> take "4 lovely"

<interactive>:12:6: error:
    • Couldn't match expected type ‘Int’ with actual type ‘[Char]’
    • In the first argument of ‘take’, namely ‘"4 lovely"’
      In the expression: take "4 lovely"
      In an equation for ‘it’: it = take "4 lovely"
Prelude> take 4 "lovely"
"love"

h) Yes

Prelude> take 3 "awesome"
"awe"

2.

a) d
b) c
c) e
d) a
e) b

Building Functions

1.

a) flip (++) "!" "Curry is awesome"

b) flip (:) [] $ flip (!!) 4 "Curry is awesome!"

c) drop 9 "Curry is awesome!"

--}

-- 2.

a :: String -> String
a = flip (++) "!"

b :: String -> String
b s = flip (:) [] $ flip (!!) 4 s

c :: String -> String
c = drop 9

-- 3

thirdLetter :: String -> Char
thirdLetter = flip (!!) 2 

-- 4

letterIndex :: Int -> Char
letterIndex = (!!) "Curry is awesome!"

-- 5

rvrs :: String -> String
rvrs s = (take 7 (drop 9 s)) ++ (take 4 (drop 5 s)) ++ (take 5 s)
