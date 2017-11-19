-- Exercises: Database Processing

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123)
           )
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123)
           )
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr ((++) . l) []
  where
    l :: DatabaseItem -> [UTCTime]
    l (DbDate x) = [x]
    l _          = []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr ((++) . l) []
  where
    l :: DatabaseItem -> [Integer]
    l (DbNumber x) = [x]
    l _            = []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = (foldr max oldUTC) . filterDbDate
  where
    oldUTC = UTCTime (fromGregorian 1 1 1) (secondsToDiffTime 1)

sumDb :: [DatabaseItem] -> Integer
sumDb =  (foldr (+) 0) . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x = sum' / numItems
  where
    sum' = (fromInteger . sumDb) x
    numItems = (fromIntegral . length . filterDbNumber) x

