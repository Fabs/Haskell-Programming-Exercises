module Reverse where

rvrs :: String -> String
rvrs s = (take 7 (drop 9 s)) ++ (take 4 (drop 5 s)) ++ (take 5 s)

main :: IO ()
main = print $ rvrs "Curry is awesome!"
