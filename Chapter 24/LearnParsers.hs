module LearnParsers where

import Text.Trifecta
import Text.ParserCombinators.ReadP ( ReadP )
import Control.Applicative

stop :: Parser a
stop = unexpected "stop"

{-
Exercises: Parsing Practice
1. Modifeied `one` and `oneTwo` to include `eof >> return ret`
-} 

-- read a single character '1'
one = char '1' >>= \ret -> eof >> return ret

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2' >>= \ret -> eof >> return ret

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop


testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"


{-
Exercises: Parsing Practice
2. Add `oneTwoThree` and `testParseString` to parse a string
-}
-- Parses up to "123"
oneTwoThree = string "123" <|> string "12" <|> string "1"
oneTwoThree' = (string "123" <|> string "12" <|> string "1") >> stop

testParseString :: Parser String -> String -> IO ()
testParseString p s = print $ parseString p mempty s

{-
Exercises: Parsing Practice
3.
-}
myStringParser :: String -> Parser String 
myStringParser (x:xs) = do
  xP <- char x
  xsP <- myStringParser xs
  return (xP:xsP)
myStringParser ([])   = return ([])

myOneTwoThree = myStringParser "123"
                <|> myStringParser "12"
                <|> myStringParser "1"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

  -- Exercises: Parsing Practice
  -- 2.
  pNL "oneTwoThree:"
  testParseString oneTwoThree "1"
  testParseString oneTwoThree "12"
  testParseString oneTwoThree "123"

  pNL "oneTwoThree':"
  testParseString oneTwoThree' "1"
  testParseString oneTwoThree' "12"
  testParseString oneTwoThree' "123"

  -- Exercises: Parsing Practice
  -- 3.
  pNL "oneTwoThree'':"
  testParseString myOneTwoThree "1"
  testParseString myOneTwoThree "12"
  testParseString myOneTwoThree "123"
