{-# OPTIONS_GHC -Wall -Werror #-}

module ParseTestCaseHelpers where

import Text.Trifecta

parseSuccessCase :: (Show a, Eq a) => Parser a -> String -> a -> IO ()
parseSuccessCase p c e = do
  putStr $ c ++ ": " 
  case parseString p mempty c of
    Success m -> do putStr "\x1b[32m" >> print (Success m)
                    if (m == e)
                    then return ()
                    else putStr "\x1b[31mexpected: " >> print e
    f -> print f
  putStr "\x1b[0m"

parseFailCase :: Parser a -> String -> IO ()
parseFailCase p c = do
  putStr $ c ++ ": " 
  case parseString p mempty c of
    Failure _ -> putStr "\x1b[32mdoesn't parse as expected."
    Success _ -> putStr "\x1b[31mshould not parse but did."
  putStrLn "\x1b[0m"
