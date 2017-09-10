module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing op = if (op 1 2) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"

mySong = sing (>)
mySong' = sing (<)
