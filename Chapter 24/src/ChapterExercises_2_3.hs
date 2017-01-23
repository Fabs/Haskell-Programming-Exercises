{-# OPTIONS_GHC -Wall -Werror #-}

module ChapterExercises_2_3 where

import Text.Trifecta
import Data.Char
import Control.Applicative
import ParseTestCaseHelpers

parseDigit :: Parser Char
parseDigit = foldr (\c acc -> char c <|> acc) (char '0') ['1'..'9']

base10Integer :: Parser Integer
base10Integer = do
  foundInts <- some $ parseDigit >>= (return . toInteger . digitToInt)
  return $ foldr (\p acc -> (acc * 10) + p) 0 $ reverse foundInts

base10Integer' :: Parser Integer
base10Integer' = (char '-' >> base10Integer >>= \i -> return (-i))
  <|> base10Integer

-- Main runs a series of tests
main :: IO ()
main = do
  putStrLn "\nTesting parseDigit:"
  parseDigitSuccessCase "123" $ '1'
  parseDigitFailCase "abc"

  putStrLn "\nTesting base10Integer:"
  parseBase10IntegerSuccessCase "123abc" $ 123
  parseBase10IntegerFailCase "abc"

  putStrLn "\nTesting base10Integer':"
  parseSuccessCase base10Integer' "-123abc" $ (-123)

parseDigitSuccessCase :: String -> Char -> IO ()
parseDigitSuccessCase = parseSuccessCase parseDigit
parseDigitFailCase :: String -> IO ()
parseDigitFailCase = parseFailCase parseDigit
parseBase10IntegerSuccessCase :: String -> Integer -> IO ()
parseBase10IntegerSuccessCase = parseSuccessCase base10Integer
parseBase10IntegerFailCase :: String -> IO ()
parseBase10IntegerFailCase = parseFailCase base10Integer 
