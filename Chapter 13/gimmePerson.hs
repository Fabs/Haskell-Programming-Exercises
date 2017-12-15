import System.IO
import System.Exit
import Data.Char

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr $ "Enter name: "
  name <- getLine
  putStr $ "Enter age: "
  age <- getLine
  if (any (not . isDigit) age)
  then do
    putStrLn "Sorry, you didn't enter an Int datatype for age. Exiting."
    exitFailure
  else
    return ()

  let p = mkPerson name $ read age
  putStrLn "Yay! Successfully got a person:"
  putStrLn $ show p
