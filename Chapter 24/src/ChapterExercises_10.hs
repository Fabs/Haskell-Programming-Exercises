{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE  QuasiQuotes #-}

module ChapterExercises_10 where

--import Control.Applicative
import Text.Trifecta
import Data.Text
import qualified Text.Dot.Types.Internal as G
import Text.RawString.QQ
import Data.Char

--import Test.Hspec
--import Test.HUnit

import ParseTestCaseHelpers

parseDotGraph :: Parser G.DotGraph
parseDotGraph = do
  skipComments
  n <- string "graph " >> parseGraphName
  return $ G.Graph G.DirectedGraph n G.DotEmpty

parseGraphName :: Parser G.GraphName
parseGraphName = do
  n <- many $ satisfy isAlphaNum
  return $ pack n

parseDot :: Parser G.Dot
parseDot = undefined

skipComments :: Parser ()
skipComments = skipMany $
  char '/' >> char '/' >> skipMany (noneOf "\n") >> skipEOL

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- Run a series of simple parsing tests
-- with fixed inputs and fixed expected results
parserTests :: IO ()
parserTests = do
  putStrLn "\nTesting Dot parser:"

  parseSuccessCase skipComments "// hello comment here  !  " $ ()
  parseSuccessCase parseGraphName "yogogogo" (pack "yogogogo")
  parseSuccessCase parseDotGraph dotGraphEx1 $
    G.Graph G.DirectedGraph (pack "graphname") G.DotEmpty

  --parseSuccessCase parseDotGraph "fefe" $
  --  G.Graph G.UndirectedGraph (pack "test") G.DotEmpty
  --parseSuccessCase parseDot "fefe" G.DotEmpty

dotGraphEx1 :: String
dotGraphEx1 = [r|// The graph name and the semicolons are optional
graph graphname {
    a -- b -- c;
    b -- d;
}
|]

