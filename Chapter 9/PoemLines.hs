module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forest of the night\n"
thirdSen = "What immortal hand or eye\n"
forthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ forthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forest of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this
myLines :: String -> [String]
myLines [] = []
myLines x = (takeLines x):(myLines $ dropLines x)
  where
    takeLines = takeWhile (/='\n')
    dropLines = (dropWhile (=='\n') . dropWhile (/='\n'))

-- What we want 'myLines sentences' to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forest of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

-- Parameterized version of myLines for question 3
mySection :: (Eq a) => a -> [a] -> [[a]]
mySection _ [] = []
mySection d x = (takeSection x):(mySection d $ dropSection x)
  where
    takeSection = takeWhile (/=d)
    dropSection = (dropWhile (==d) . dropWhile (/=d))

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main = do
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
  print $ "(mySection) Are they equal? " ++ show (mySection '\n' sentences == shouldEqual)
