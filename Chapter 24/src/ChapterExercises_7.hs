{-# OPTIONS_GHC -Wall -Werror #-}

module ChapterExercises_7 where

import Control.Applicative
import Text.Trifecta
import Data.Word
import Numeric
import Data.Bits

import Test.Hspec
import Test.HUnit

import ParseTestCaseHelpers

data IPAddress6 = IPAddress6 Word64 Word64
  deriving (Eq, Ord)

-- Show IPAddress6 in expanded form
instance Show IPAddress6 where
  show (IPAddress6 w1 w2) =
    let sg :: Word64 -> Int -> String
        sg w i = showHex (shiftR w (16*i)
                       .&. (toEnum . fromEnum) (maxBound :: Word16)) ""
    in  sg w1 3  ++ ":" ++ sg w1 2 ++ ":" ++ sg w1 1 ++ ":" ++ sg w1 0
        ++ ":" ++ sg w2 3  ++ ":" ++ sg w2 2 ++ ":" ++ sg w2 1 ++ ":"
        ++ sg w2 0


ipv6toInteger :: IPAddress6 -> Integer
ipv6toInteger (IPAddress6 w1 w2) =
  ((65536 ^ (4::Integer)) * (toInteger w1))
  + ((65536 ^ (0::Integer)) * (toInteger w2))

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = parseIPv6Sections >>= return . calcIPAddress6

calcIPAddress6 :: [IPv6Section] -> IPAddress6
calcIPAddress6 s =
  let sExp = expandIPv6Sections s
      calculateGroups :: [IPv6Section] -> (Word64, Integer)
      calculateGroups secs =
        foldr (\(Group i) (w, p) -> (w + ( (toEnum $ fromEnum i)
                                         * (65536 ^ p)),p+1)) (0,0) secs
      (firstWord64, _) = calculateGroups (take 4 sExp)
      (secondWord64, _) = calculateGroups (drop 4 sExp)
  in IPAddress6 firstWord64 secondWord64

-- Assumes only one 'Collapse' exists  
expandIPv6Sections :: [IPv6Section] -> [IPv6Section]
expandIPv6Sections sections =
  let maxSize = 8
      s = length sections
      zg = replicate (maxSize + 1 - s) $ Group 0
  in foldr (\sec ac -> if sec == Collapse
                     then zg ++ ac
                     else (sec:ac)) [] sections

data IPv6Section = Group Word16 | Collapse
  deriving (Eq, Show)

parseIPv6Sections :: Parser [IPv6Section]
parseIPv6Sections = do
  -- Parse a list of Groups and Collapses as is.
  start <- (  (do IPv6Group x <- parseIPv6Group
                  pure $ Group x)
              <* char ':'
          <|> (string "::" >> pure Collapse)
           ) <?> "Start of IPv6"
  middle <- many $ try (  
                          (do IPv6Group x <- parseIPv6Group
                              pure $ Group x)
                          <* char ':'
                      <|> (char ':' >> pure Collapse)
                       ) <?> "Middle of IPv6"
  end <- (  (do IPv6Group x <- parseIPv6Group
                pure $ Group x)
        <|> (char ':' >> pure Collapse)
         ) <?> "End of IPv6"

  let sl = [start] ++ middle ++ [end]

  -- If more than one Collapse exists then we have a problem
  if (foldr (\s acc -> if (s == Collapse)
                       then acc+1
                       else acc) (0::Int) sl) > 1
  then unexpected "Too many collapsed sections" 
  else return ()
  
  if length sl > 16
  then unexpected "Too many sections"
  else return ()

  return sl

newtype IPv6Group = IPv6Group Word16
  deriving (Eq, Ord, Show)

parseIPv6Group :: Parser IPv6Group
parseIPv6Group = do
  -- Take dangerous shortcuts to get parsed hex value.
  s <- some hexDigit :: Parser String
  if length s > 4
    then unexpected $ "Group of (" ++ (show $ length s)
                                   ++ ") size is too large"
    else return ()
  let ((g,_):_) = readHex s :: [(Integer, String)]
  return $ IPv6Group $ toEnum $ fromInteger g

-- Run a series of simple parsing tests
-- with fixed inputs and fixed expected results
parserTests :: IO ()
parserTests = do
  putStrLn "\nTesting IPv6 parser:"

  parseFailCase parseIPv6Group ""
  parseFailCase parseIPv6Group ":"
  parseSuccessCase parseIPv6Group "0000" $ IPv6Group 0
  parseSuccessCase parseIPv6Group "000a" $ IPv6Group 10
  parseSuccessCase parseIPv6Group "00fa" $ IPv6Group 250
  parseSuccessCase parseIPv6Group "10fa" $ IPv6Group 4346
  parseFailCase parseIPv6Group "11111"

  parseSuccessCase parseIPv6Sections "0000:001" $ [Group 0, Group 1]
  parseSuccessCase parseIPv6Sections "0000:1:001" $
    [Group 0, Group 1, Group 1]
  parseSuccessCase parseIPv6Sections "0000:1::001" $
    [Group 0, Group 1, Collapse, Group 1]
  parseSuccessCase parseIPv6Sections "0000:1::001" $
    [Group 0, Group 1, Collapse, Group 1]
  parseFailCase parseIPv6Sections "1:1::1::11"


-- Tests other non parsing functions
testIPv6Helpers :: IO ()
testIPv6Helpers = hspec $ -- do
  describe "IPv6" $ do
    it "Expanding collapsed IPv6Section (middle)" $
      let d = [Group 1, Collapse, Group 1]
      in  expandIPv6Sections d
          @?=
            [ Group 1, Group 0, Group 0, Group 0
            , Group 0, Group 0, Group 0, Group 1  
            ]
    it "Expanding collapsed IPv6Section (start)" $
      let d = [Collapse, Group 100, Group 1]
      in  expandIPv6Sections d
          @?=
            [ Group 0, Group 0, Group 0, Group 0
            , Group 0, Group 0, Group 100, Group 1  
            ]
    it "Expanding collapsed IPv6Section (end)" $
      let d = [Group 1002, Group 1, Collapse]
      in  expandIPv6Sections d
          @?=
            [ Group 1002, Group 1, Group 0, Group 0
            , Group 0, Group 0, Group 0, Group 0  
            ]
    it "calculate '::1' IPv6 value" $
      let d = [Collapse, Group 1]
      in  calcIPAddress6 d `shouldBe` IPAddress6 0 1
    it "calculate '::2' IPv6 value" $
      let d = [Collapse, Group 2]
      in  calcIPAddress6 d `shouldBe` IPAddress6 0 2
    it "calculate '::0' IPv6 value" $
      let d = [Collapse, Group 0]
      in  calcIPAddress6 d `shouldBe` IPAddress6 0 0

    it "calculate '0:ffff:cc78:f:0:ffff:ac10:fe01' IPv6 value" $
      let d = [ Group 0, Group 65535, Group 52344, Group 15
              , Group 0, Group 65535, Group 44048, Group 65025 ]
      in  calcIPAddress6 d `shouldBe`
            IPAddress6 281474112159759 281473568538113 

    it "'0:0:0:0:0:ffff:ac10:fe01' IPv6 to decimal" $ do
      let (Success p) =
            parseString parseIPAddress6 mempty "0:0:0:0:0:ffff:ac10:fe01"
      ipv6toInteger p `shouldBe` 281473568538113
    it "'0:0:0:0:0:ffff:cc78:f' IPv6 to decimal" $ do
      let (Success p) =
            parseString parseIPAddress6 mempty "0:0:0:0:0:ffff:cc78:f"
      ipv6toInteger p `shouldBe` 281474112159759
    it "'FE80:0000:0000:0000:0202:B3FF:FE1E:8329' IPv6 to decimal" $ do
      let (Success p) =
            parseString parseIPAddress6 mempty
              "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
      ipv6toInteger p `shouldBe` 338288524927261089654163772891438416681
    it "'2001:DB8::8:800:200C:417A' IPv6 to decimal" $ do
      let (Success p) =
            parseString parseIPAddress6 mempty "2001:DB8::8:800:200C:417A"
      ipv6toInteger p `shouldBe` 42540766411282592856906245548098208122
    it "'FE80::0202:B3FF:FE1E:8329' IPv6 to decimal" $ do
      let (Success p) =
            parseString parseIPAddress6 mempty "FE80::0202:B3FF:FE1E:8329"
      ipv6toInteger p `shouldBe` 338288524927261089654163772891438416681
    it "'::4' IPv6 to decimal" $ do
      let (Success p) = parseString parseIPAddress6 mempty "::4"
      ipv6toInteger p `shouldBe` 4
