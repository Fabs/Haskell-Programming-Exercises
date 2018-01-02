module Main where

import System.Environment
import Sum
import Nope
import PhhhbbtttEither
import Identity
import List

main :: IO ()
main = getArgs >>= checkBasedOnArgs

checkBasedOnArgs :: [String] -> IO ()
checkBasedOnArgs ("Sum":_) = checkSum
checkBasedOnArgs ("Nope":_) = checkNope
checkBasedOnArgs ("PhhhbbtttEither":_) = checkPhhhbbtttEither
checkBasedOnArgs ("Identity":_) = checkIdentityEither
checkBasedOnArgs ("List":_) = checkListEither
checkBasedOnArgs (x:_) = do
  putStrLn $ "First argument '" ++ x ++ "' didn't match a check."
  putStrLn $ "Try one of the following:\n Sum\n Nope\n PhhhbbtttEither\n List"
checkBasedOnArgs [] = do
  putStrLn "Enter one of these test names as argument:"
  putStrLn "  Sum"
  putStrLn "  Nope"
  putStrLn "  PhhhbbtttEither"
  putStrLn "  Identity"
  putStrLn "  List"
