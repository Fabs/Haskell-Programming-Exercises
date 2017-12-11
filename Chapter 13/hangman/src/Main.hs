module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- I'm also filtering out words with 'bad' characters
-- which appear in my wordlist
gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList $ ((filter goodCharsOnly) . (filter gameLength)) aw
  where gameLength w =
          let l = length (w :: String)
          in     l >= minWordLength
              && l < maxWordLength
        goodCharsOnly w =
          let badChars = ['0'..'9'] ++ "'"
          in  foldr (\x acc -> if acc || elem x w
                               then False
                               else True) False badChars

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0,length wl)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s) c = elem c s

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c 
renderPuzzleChar Nothing  = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of

    (_, True) -> do
      putStrLn "You already guesed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      if (isGameOver puzzle)
      then putStrLn "This character wasn't in the word, try again."
      else putStrLn "This character wasn't in the word."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle wordToGuess _ _) =
  if isGameOver p then do
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

isGameOver :: Puzzle -> Bool
isGameOver p = if (numWrongGuesses p) > 7 then True else False

-- Determine how many wrong guesses there are
numWrongGuesses :: Puzzle -> Int
numWrongGuesses (Puzzle wordToGuess _ guesses) =
  foldr (\x c -> if elem x wordToGuess then c else c+1) 0 guesses

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  -- See if the puzzle is complete first
  gameWin puzzle
  -- Then see if they have had the final guess
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character" 

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
