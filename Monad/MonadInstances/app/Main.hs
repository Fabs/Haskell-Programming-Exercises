module Main where

import System.Environment
import Sum
import Nope
import PhhhbbtttEither

main :: IO ()
main = getArgs >>= checkBasedOnArgs

checkBasedOnArgs :: [String] -> IO ()
checkBasedOnArgs ("Sum":_) = checkSum
checkBasedOnArgs ("Nope":_) = checkNope
checkBasedOnArgs ("PhhhbbtttEither":_) = checkPhhhbbtttEither
checkBasedOnArgs (x:_) = do
  putStrLn $ "First argument '" ++ x ++ "' didn't match a check."
  putStrLn $ "Try one of the following:\n Sum\n Nope\n PhhhbbtttEither"
