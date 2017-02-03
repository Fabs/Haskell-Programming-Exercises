{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE  QuasiQuotes #-}

module ChapterExercises_5 where

import Text.Trifecta
import ParseTestCaseHelpers
import Data.Char
import Control.Applicative
import Text.RawString.QQ
import Data.List
import Test.Hspec
import Numeric
import Text.Printf
import qualified Test.QuickCheck as QC
import Data.Maybe


-- Parsing Log datatypes

data Log = Log [Section]
  deriving (Eq)

instance Show Log where
  show (Log sections) = foldr (\s a -> (show s) ++ a) "" sections

instance QC.Arbitrary Log where
  arbitrary = do
    ss <- QC.listOf1 QC.arbitrary
    return $ Log ss

parseLog :: Parser Log
parseLog = Log <$> many parseSection

data Section = Section Date [ActivityEntry]
  deriving (Eq)

instance Show Section where
  show (Section (Date y m d) aes) =
    (printf "# %02d-%02d-%02d\n" y m d) ++
    foldr (\ae a -> (show ae) ++ a) "" aes ++ "\n"

instance QC.Arbitrary Section where
  arbitrary = do
    d <- QC.arbitrary
    aes <- QC.listOf1 QC.arbitrary
    return $ Section d aes

parseSection :: Parser Section
parseSection = try $ do
  _ <- skipOptional $ spaces >> skipComments
  d <- parseDateLine
  as <- some parseActivityEntry
  return $ Section d as

type Activity = String
data ActivityEntry = ActivityEntry Time Activity
  deriving (Eq)

instance Show ActivityEntry where
  show (ActivityEntry (Time h m) a) =
    printf "%02d:%02d %s\n" h m a

instance QC.Arbitrary ActivityEntry where
  arbitrary = do
    t <- QC.arbitrary
    c <- QC.listOf1 $ QC.suchThat QC.arbitrary
           (\x -> isAlphaNum x || (elem x ","))
    return $ ActivityEntry t c

parseActivityEntry :: Parser ActivityEntry
parseActivityEntry = do
  t <- parseTime
  _ <- char ' '
  a <- parseWords
  skipEndLineComments
  return $ ActivityEntry t a

-- Parse short description of activity
-- Each word must start with an 
parseWords :: Parser String
parseWords = do
  w <- parseWord
  ws <- many ( try $ do
               s <- some $ char ' '
               w' <- parseWord
               return $ s ++ w'
             ) 
  return $ w ++ (intercalate "" ws)
 
parseWord :: Parser String
parseWord = do
  wordStart  <- validWordStart
  wordFinish <- many $ (satisfy isAlphaNum) <|>
                       (oneOf $ safeChars ++ "-")
  return $ wordStart ++ wordFinish
 where
   validWordStart =
     ( try $ do
       c1 <- satisfy isAlphaNum <|> (oneOf $ safeChars ++ "-")
       c2 <- satisfy isAlphaNum <|> (oneOf $ safeChars ++ " ")
       return [c1,c2]
     ) <|>
     ( do
       c1 <- satisfy isAlphaNum <|> (oneOf safeChars)
       return [c1]
     )
   safeChars = ",&"

type Hour = Int; type Minute = Int
data Time = Time Hour Minute
  deriving (Eq, Show)

instance QC.Arbitrary Time where
  arbitrary = do
    h <- QC.elements [0..23]
    m <- QC.elements [0..59]
    return (Time h m)

parseTime :: Parser Time
parseTime = do
  h <- parseDigits 2
  _ <- char ':'
  m <- parseDigits 2
  return $ Time h m

parseDateLine :: Parser Date
parseDateLine = do
  _ <- char '#'
  _ <- skipMany (char ' ')
  d <- parseDate
  skipEndLineComments
  return d

type Year = Int; type Month = Int; type Day = Int;
data Date = Date Year Month Day
  deriving (Eq, Show)

instance QC.Arbitrary Date where
  arbitrary = do
    y <- QC.elements [1000..9999]
    m <- QC.elements [1..12]
    d <- QC.elements [1..31]
    return (Date y m d)

parseDate :: Parser Date
parseDate = do
  y <- parseDigits 4
  _ <- char '-'
  m <- parseDigits 2
  _ <- char '-'
  d <- parseDigits 2
  return $ Date y m d
  
parseDigits :: Int -> Parser Int
parseDigits x = count x (digit) >>= return . read

skipEndLineComments :: Parser ()
skipEndLineComments = skipOptional $ space >> skipComments
skipComments :: Parser ()
skipComments = skipMany $
  char '-' >> char '-' >> skipMany (noneOf "\n") >> skipEOL

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- Run a series of simple parsing tests
-- with fixed inputs and fixed expected results
parserTests :: IO ()
parserTests = do
  putStrLn "\nTesting parseLogfile:"

  parseSuccessCase skipComments "-- hello" $ ()
  parseSuccessCase parseDate "1980-04-21" $ Date 1980 04 21
  parseSuccessCase parseDateLine "# 1980-04-21" $ Date 1980 04 21
  parseSuccessCase parseDateLine "# 1980-04-21 -- Comment here" $
    Date 1980 04 21
  parseSuccessCase parseTime "23:59" $ Time 23 59
  parseSuccessCase parseActivityEntry "01:00 Hello sir" $
    ActivityEntry (Time 1 0) "Hello sir"
  parseSuccessCase parseActivityEntry "01:33 Hello sir -- With comment" $
    ActivityEntry (Time 1 33) "Hello sir"
  parseSuccessCase parseActivityEntry "00:01 t" $
    ActivityEntry (Time 0 1) "t"
  parseSuccessCase parseWords "one two 3 gogogo -- Comment here"
                              "one two 3 gogogo"
  parseSuccessCase parseWords "plz thanks" "plz thanks"
  parseSuccessCase parseWords "t" "t"
  parseSuccessCase parseWord "R&R" "R&R"
  parseSuccessCase parseWord "RRR&R" "RRR&R"
  parseSuccessCase parseWord "R" "R"

  parseSuccessCase parseSection case1 $
    Section (Date 2025 02 05)
      [ ActivityEntry (Time 8 0) "Breakfast"
      , ActivityEntry (Time 9 0) "Sanitizing moisture collector"
      ]

  parseSuccessCase parseSection case2 $
    Section (Date 2025 02 07)
      [ ActivityEntry (Time 8 0) "Breakfast"
      , ActivityEntry (Time 9 0) "Bumped head, passed out"
      ]
  parseSuccessCase parseLog case3 $
    Log
      [ Section (Date 2025 02 07)
        [ ActivityEntry (Time 8 0) "B"
        ]
      ]


  parseSuccessCase parseLog logFile $
    Log [ Section (Date 2025 02 05)
          [ ActivityEntry (Time 8 0) "Breakfast"
          , ActivityEntry (Time 9 0) "Sanitizing moisture collector"
          , ActivityEntry (Time 11 0) "Exercising in high-grav gym"
          , ActivityEntry (Time 12 0) "Lunch"
          , ActivityEntry (Time 13 0) "Programming"
          , ActivityEntry (Time 17 0) "Commuting home in rover"
          , ActivityEntry (Time 17 30) "R&R"
          , ActivityEntry (Time 19 0) "Dinner"
          , ActivityEntry (Time 21 0)  "Shower"
          , ActivityEntry (Time 21 15) "Read"
          , ActivityEntry (Time 22 0)  "Sleep"
          ]
        , Section (Date 2025 02 07)
          [ ActivityEntry (Time 08 00) "Breakfast"
          , ActivityEntry (Time 09 00) "Bumped head, passed out"
          , ActivityEntry (Time 13 36) "Wake up, headache"
          , ActivityEntry (Time 13 37) "Go to medbay"
          , ActivityEntry (Time 13 40) "Patch self up"
          , ActivityEntry (Time 13 45) "Commute home for rest"
          , ActivityEntry (Time 14 15) "Read"
          , ActivityEntry (Time 21 00) "Dinner"
          , ActivityEntry (Time 21 15) "Read"
          , ActivityEntry (Time 22 00) "Sleep"
          ]
        ]

case1 :: String
case1 = [r|-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector|]

case2 :: String
case2 = [r|-- Another comment?
-- Yeah

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out

-- More comments at random|]

-- Multiple blank lines at the end of string
case3 :: String
case3 = [r|# 2025-02-07
08:00 B

|]

logFile :: String
logFile = [r|-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

-- Helper function for pulling out the parsed result into a Maybe
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing 

-- Print the activity time spent for each activity in the log
-- given in the exercise
printSumTimeForEachActivity :: IO ()
printSumTimeForEachActivity = do
  let l = maybe ([]) sumTimeForEachActivity $
            maybeSuccess $ parseString parseLog mempty logFile
  putStrLn $ show l

type SectionSum = [ActivitySum]
type ActivitySum = (Activity,Minute)

sumUpSection :: Section -> SectionSum
sumUpSection (Section _ a) =
  (sumBeforeFirstActivity a) ++ (sumUpActivityEntries a)

-- Get the time before the first activity
sumBeforeFirstActivity :: [ActivityEntry] -> SectionSum
sumBeforeFirstActivity ((ActivityEntry (Time h m) _):_) =
  [("Before first activity", (h*60 + m))]
sumBeforeFirstActivity [] = [("No activity today", 24*60)]

sumUpActivityEntries :: [ActivityEntry] -> SectionSum
sumUpActivityEntries
  ((ActivityEntry (Time h1 m1) s1):(ActivityEntry (Time h2 m2) s2):as) =
  [(s1, ((h2-h1)*60)+(m2-m1))] ++ sumUpActivityEntries
    ((ActivityEntry (Time h2 m2) s2):as)
sumUpActivityEntries
  ((ActivityEntry (Time h m) s):[]) = [(s, ((23-h)*60)+60-m)]
sumUpActivityEntries [] = []

combineDuplicateSectionSums :: SectionSum -> SectionSum
combineDuplicateSectionSums =
  foldl (\acc as -> addActivitySumToSectionSum as acc) []

addActivitySumToSectionSum :: ActivitySum -> SectionSum -> SectionSum
addActivitySumToSectionSum (a, t) ((sa,st):ss) 
  | a == sa = ((sa,st+t):ss)
  | otherwise = (sa,st):(addActivitySumToSectionSum (a,t) ss)
addActivitySumToSectionSum as [] = [as]

sumTimeForEachActivity :: Log -> SectionSum
sumTimeForEachActivity (Log sections) =
   combineDuplicateSectionSums $ concatMap sumUpSection sections

-- Hspec test to verify functionality of the activity summing
testSectionSum :: IO ()
testSectionSum = hspec $ do 
  describe "Log Activity Summing" $ do
    it "can sum up a section" $ do
      let m = parseString parseSection mempty case1
          r' = maybeSuccess m
      case r' of
        Just s -> sumUpSection s `shouldBe`
                  [ ("Before first activity",480)
                  , ("Breakfast",60)
                  , ("Sanitizing moisture collector",900)
                  ]
        Nothing -> putStrLn "Failed to parse"
    it "can combine duplicate SectionSums" $ do
      let m = parseString parseSection mempty case1
          r' = maybeSuccess m
      case r' of
        Just s -> combineDuplicateSectionSums
                    ((sumUpSection s) ++ (sumUpSection s)) `shouldBe`
                  [ ("Before first activity",480*2)
                  , ("Breakfast",60*2)
                  , ("Sanitizing moisture collector",900*2)
                  ]
        Nothing -> putStrLn "Failed to parse"

-- Get the average of each activity,
-- includes time before first activity as an activity
activityAverage :: SectionSum -> Rational
activityAverage ss =
  let sum' = foldr (\(_, t) a -> t + a) 0 ss
  in  (toRational sum') / (toRational (length ss))

-- Hspec test to verify averaging functionality
testSectionActivityAverage :: IO ()
testSectionActivityAverage = hspec $ --do 
  describe "Activity Averages" $ do
    it "can be averaged" $ do
      let m = parseString parseSection mempty case1
          r' = maybeSuccess m
      case r' of
        Just s -> let sums = sumUpSection s
                  in  (activityAverage $ sumUpSection s) `shouldBe`
                  ((24*60) / toRational (length sums))
        Nothing -> putStrLn "Failed to parse"

-- Print the average of log file example as a Double
printAverageForEachActivity :: IO ()
printAverageForEachActivity = do
  let avg = maybe (0) (activityAverage . sumTimeForEachActivity) $
            maybeSuccess $ parseString parseLog mempty logFile
  putStrLn $ showFloat (fromRat avg::Double) ""

-- Verify Bidirectional parsing is working for some the constructed
-- Arbitrary Log
testBidirectionalParsing :: IO ()
testBidirectionalParsing = hspec $ do 
  describe "Bidirectional Parsing" $ do
    it "Arbitrary Date is within acceptable ranges" $ do
      QC.property $ \(Date y m d) ->
        ( (y >= 1000) && (y <= 9999) -- year
       && (m >= 1) && (m <= 12) -- month
       && (d >= 1) && (d <= 31) -- day
        )
    it "Arbitrary Time is within acceptable ranges" $ do
      QC.property $ \(Time h m) ->
        ( (h >= 0) && (h <= 23) -- hour
       && (m >= 0) && (m <= 59) -- minute
        )
    it "QuickCheck bidirectional parsing" $ do
      QC.property $ \l ->
        let lsp = fromMaybe (Log []) $ maybeSuccess $
                    parseString parseLog mempty $ show (l::Log)
        in  lsp == l

    it "Can parse Arbitrary Log" $ do
      QC.property $ \l ->
        let pr = parseString parseLog mempty $ show (l::Log)
            --pr = parseString parseLog mempty "# 5790-06-07\n19:29 k"
        in  case pr of
              Success _ -> True
              Failure _ -> False
