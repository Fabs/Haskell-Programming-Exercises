module UnitOfSuccess where

import Text.Trifecta

yourFuncHere :: Parser Integer
yourFuncHere = integer >>= \i -> eof >> return i

main = putStrLn "Hello"
