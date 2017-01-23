{-# OPTIONS_GHC -Wall -Werror #-}

module ChapterExercises_4 where

import Text.Trifecta
import ParseTestCaseHelpers
--import Data.Char
--import Data.String

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  _ <- optional $ try $ char '1' >> char '-'
  _ <- optional $ char '('
  numberingPlanArea <- parseDigits 3
  _ <- optional $ char ')'; _ <- optional spaces; _ <- optional $ char '-'
  exchange <- parseDigits 3
  _ <- optional $ char '-'
  lineNumber <- parseDigits 4
  return $ PhoneNumber numberingPlanArea exchange lineNumber

parseDigits :: Int -> Parser Int
parseDigits x = count x (digit) >>= return . read

-- Main runs a series of tests
main :: IO ()
main = do
  putStrLn "\nTesting parsePhone:"
  parseSuccessCase parsePhone "123-456-7890" $ PhoneNumber 123 456 7890
  parseSuccessCase parsePhone "1234567890" $ PhoneNumber 123 456 7890
  parseSuccessCase parsePhone "(123) 456-7890" $ PhoneNumber 123 456 7890
  parseSuccessCase parsePhone "1-123-456-7890" $ PhoneNumber 123 456 7890
