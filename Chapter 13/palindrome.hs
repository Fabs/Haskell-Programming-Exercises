import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

palindrome' :: IO ()
palindrome' = forever $ do
  -- Get input. Clear out spaces, and punctuation. Change all caps to lowercase.
  line1 <- getLine >>= (return . filter isLetter .  map toLower)
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
