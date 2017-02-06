{-# OPTIONS_GHC -Wall -Werror #-}

module ChapterExercises_6 where

import Text.Trifecta
import Data.Word
import ParseTestCaseHelpers

newtype Octet = Octet Word8
  deriving (Eq, Show)

newtype IPAddress = IPAddress Word32
  deriving (Eq, Ord, Show)

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  (Octet o1) <- parseOctet
  _ <- char '.'
  (Octet o2) <- parseOctet
  _ <- char '.'
  (Octet o3) <- parseOctet
  _ <- char '.'
  (Octet o4) <- parseOctet

  return $ IPAddress $
    toEnum ( ((fromEnum o1) * 256^(3::Int))
           + ((fromEnum o2) * 256^(2::Int))
           + ((fromEnum o3) * 256^(1::Int))
           + ((fromEnum o4) * 256^(0::Int))
           )

parseOctet :: Parser Octet
parseOctet = do
  o <- (read <$> some digit :: Parser Int)

  if o > 255
  then unexpected "Octet greater than 255"
  else return $ Octet $ (toEnum o)

-- Run a series of simple parsing tests
-- with fixed inputs and fixed expected results
parserTests :: IO ()
parserTests = do
  putStrLn "\nTesting IPv4 parser:"

  parseSuccessCase parseOctet "244" $ Octet 244
  parseFailCase parseOctet "256"
  parseSuccessCase parseOctet "0" $ Octet 0
  parseFailCase parseOctet "-1"

  parseSuccessCase parseIPAddress "172.16.254.1" $ IPAddress 2886794753
  parseSuccessCase parseIPAddress "204.120.0.15" $ IPAddress 3430416399
  parseFailCase parseIPAddress "256.1.1.1"
