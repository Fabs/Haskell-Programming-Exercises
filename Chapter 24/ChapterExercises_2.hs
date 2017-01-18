{-# OPTIONS_GHC -Wall #-}

import Text.Trifecta
import Data.Char
import Control.Applicative

parseDigit :: Parser Char
parseDigit = foldr (\c acc -> char c <|> acc) (char '0') ['1'..'9']

base10Integer :: Parser Integer
base10Integer = do
  foundInts <- some $ parseDigit >>= (return . toInteger . digitToInt)
  return $ foldr (\p acc -> (acc * 10) + p) 0 $ reverse foundInts

-- Main runs a series of tests
main :: IO ()
main = do
  putStrLn "\nTesting parseDigit:"
  parseDigitSuccessCase "123" $ '1'
  parseDigitFailCase "abc"

  putStrLn "\nTesting base10Integer:"
  parseBase10IntegerSuccessCase "123abc" $ 123
  parseBase10IntegerFailCase "abc"

parseDigitSuccessCase :: String -> Char -> IO ()
parseDigitSuccessCase = parseSuccessCase parseDigit
parseDigitFailCase :: String -> IO ()
parseDigitFailCase = parseFailCase parseDigit
parseBase10IntegerSuccessCase :: String -> Integer -> IO ()
parseBase10IntegerSuccessCase = parseSuccessCase base10Integer
parseBase10IntegerFailCase :: String -> IO ()
parseBase10IntegerFailCase = parseFailCase base10Integer 

parseSuccessCase :: (Show a, Eq a) => Parser a -> String -> a -> IO ()
parseSuccessCase p c e = do
  putStr $ c ++ ": " 
  case parseString p mempty c of
    Success m -> do putStr "\x1b[32m" >> print (Success m)
                    if (m == e)
                    then return ()
                    else putStr "\x1b[31mexpected: " >> print e
    f -> print f
  putStr "\x1b[0m"

parseFailCase :: Parser a -> String -> IO ()
parseFailCase p c = do
  putStr $ c ++ ": " 
  case parseString p mempty c of
    Failure _ -> putStr "\x1b[32mdoesn't parse as expected."
    Success _ -> putStr "\x1b[31mshould not parse but did."
  putStrLn "\x1b[0m"
