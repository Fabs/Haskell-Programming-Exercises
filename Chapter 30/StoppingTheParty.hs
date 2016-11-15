module StoppingTheParty where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import System.Random (randomRIO)

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if i `elem` [1..9]
    then throwIO DivideByZero
    else throwIO StackOverflow

main :: IO ()
main = forever $ do
  -- Changing 'ArithException' to 'SomeException' will catch all exceptions.
  -- I'm not sure how to catch only 'DivideByZero' and 'StackOverflow' easily.
  --let tryS :: IO () -> IO (Either ArithException ())
  let tryS :: IO () -> IO (Either SomeException ())
      tryS = try
  e <- tryS randomException
  putStrLn $ (show e) ++ ", but live to loop another day!"
  -- microseconds
  threadDelay (1 * 1000000)
