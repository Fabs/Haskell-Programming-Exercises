{-# OPTIONS_GHC -Wall -Werror #-}

module ChapterExercises_1 where

import Text.Trifecta
import Data.List
import Data.Char
import Control.Applicative

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString = NOSS String
                    | NOSI Integer
  deriving (Show, Eq)

instance Ord NumberOrString where
  (<=) (NOSI _) (NOSS _) = True
  (<=) (NOSS _) (NOSI _) = False
  (<=) (NOSI x) (NOSI y) = x <= y
  (<=) (NOSS x) (NOSS y) = x <= y

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  (<=) (SemVer maj1 min1 p1 rel1 _) (SemVer maj2 min2 p2 rel2 _)
    | maj1 /= maj2 = maj1 <= maj2
    | min1 /= min2 = min1 <= min2
    | p1 /= p2     = p1 <= p2
    | p1 /= p2     = p1 <= p2
    | rel1 == []   = False
    | rel1 /= rel2 = rel1 <= rel2
  (<=) (SemVer _ _ _ _ _) (SemVer _ _ _ _ _) = undefined

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  minor <- char '.' >> decimal
  patch <- char '.' >> decimal
  release' <- try $ ((char '-' >> parseNumberOrStringList) <|> return []) 
  metadata <- try $ ((char '+' >> parseNumberOrStringList) <|> return [])
  eof
  return $ SemVer major minor patch release' metadata

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = (try decimal >>= return . NOSI)
                  <|> (try $ some (satisfy isAlphaNum) >>= return . NOSS)

parseNumberOrStringList :: Parser [NumberOrString]
parseNumberOrStringList = sepBy parseNumberOrString (char '.') 

-- Tests
ver1 :: SemVer
ver1 = SemVer 2 1 1 [] []
ver2 :: SemVer
ver2 = SemVer 2 1 0 [] []

-- Main runs a series of tests
main :: IO ()
main = do
  putStrLn "\nTesting parser"
  parseSuccessCase "2.1.1" $ SemVer 2 1 1 [] []
  parseFailCase "1"
  parseFailCase "1.0"
  parseSuccessCase "1.0.0" $ SemVer 1 0 0 [] []
  parseSuccessCase "1.0.0-1.alpha" $ SemVer 1 0 0 [NOSI 1, NOSS "alpha"] []
  parseSuccessCase "1.0.0-1.beta+patch.here" $ SemVer 1 0 0 [NOSI 1, NOSS "beta"] [NOSS "patch", NOSS "here"]
  parseSuccessCase "1.0.0+patch.here" $ SemVer 1 0 0 [] [NOSS "patch", NOSS "here"]
  --case parseString parseSemVer mempty case1 of
  --  Failure _ -> return ()
  --  Success _ -> putStrLn $ case1 ++ " should not parse."

  putStrLn "\nTesting order"
  verifyGreaterThan ver1 ver2

  putStrLn "\nSorting versions"
  print $ sort [ver1, ver2, ver1, ver1]
  print $ sort [ SemVer 9 9 9 [] [NOSS "wow", NOSI 1000]
               , SemVer 1 0 0 [NOSS "alpha"] []
               , SemVer 1 0 0 [NOSS "alpha", NOSI 1] []
               , SemVer 1 0 0 [NOSS "beta"] []
               , SemVer 1 0 0 [] []
               , SemVer 1 0 0 [NOSS "beta", NOSI 2] []
               , SemVer 1 0 0 [NOSS "beta", NOSI 11] []
               , SemVer 1 0 0 [NOSS "rc", NOSI 1] []
               , SemVer 1 0 0 [NOSS "alpha", NOSS "beta"] []
               ]

  putStrLn "\nExpected Results"
  print $ parseString parseSemVer mempty "2.1.1" 
  print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
  print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []

parseSuccessCase :: String -> SemVer -> IO ()
parseSuccessCase c expected' = do
  putStr $ c ++ ": " 
  case parseString parseSemVer mempty c of
    Success m -> do putStrLn "\x1b[32m" >> print (Success m)
                    if (m == expected')
                    then return ()
                    else putStr "\x1b[31mexpected: " >> print expected'
    f -> print f
  putStr "\x1b[0m"

parseFailCase :: String -> IO ()
parseFailCase c = do
  putStr $ c ++ ": " 
  case parseString parseSemVer mempty c of
    Failure _ -> putStr "\x1b[32mdoesn't parse as expected."
    Success _ -> putStr "\x1b[31mshould not parse but did."
  putStrLn "\x1b[0m"

verifyGreaterThan :: SemVer -> SemVer -> IO ()
verifyGreaterThan v1 v2 = do
  putStr $ (show v1) ++ " > " ++ (show v2) ++ ": "
  if v1 > v2
  then putStr "\x1b[32mTrue" 
  else putStr "\x1b[31mFalse"
  putStrLn "\x1b[0m"
