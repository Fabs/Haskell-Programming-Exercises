{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE  QuasiQuotes #-}

module ChapterExercises_10 where

import Control.Applicative
import Text.Trifecta
import Data.Text
import qualified Text.Dot.Types.Internal as G
import Text.RawString.QQ
import Data.Char
import Data.Monoid ((<>))

--import Test.Hspec
--import Test.HUnit

import ParseTestCaseHelpers

parseDotGraph :: Parser G.DotGraph
parseDotGraph = do
  _ <- skipOptional $ spaces >> skipComments
  n <- string "graph " >> parseGraphName
  char ' ' >> char '{' >> skipEOL
  d <- parseDot
  return $ G.Graph G.DirectedGraph n d

parseGraphName :: Parser G.GraphName
parseGraphName = do
  n <- many $ satisfy isAlphaNum
  return $ pack n

-- | Needs a lot of work to work correctly. Probably need to
-- be broken up in to several specific DOT parsers such as `parseDotEdgeLine`
parseDot :: Parser G.Dot
parseDot =
  -- End of graph
  ( try $ do
    _ <- skipOptional $ spaces
    _ <- char '}'
    return $ G.DotEmpty
  ) <|>
  -- Single Node
  ( try $ do
    n <- many $ satisfy isAlphaNum
    _ <- char ';'
    skipEOL
    ns <- parseDot

    return $ (G.Node (G.UserId (pack n)) []) <> ns
  ) <|>
  -- Edge
  ( try $ do
    n1 <- many $ satisfy isAlphaNum
    _ <- string " -- "
    ns <- parseDot

    -- Get the second UserId of the Edge
    case ns of
      (G.Node (G.UserId t) []) ->
        return $ (G.Edge (G.UserId (pack n1)) (G.UserId t) []) <> ns
      (G.Edge (G.UserId t) _ []) ->
        return $ (G.Edge (G.UserId (pack n1)) (G.UserId t) []) <> ns
      _ -> unexpected "Need to handle more cases of Dot graph"
  )

parseDotEdgeLine :: Parser [G.NodeId]
parseDotEdgeLine =
  -- Starting edge
  ( try $ do
    n <- some $ satisfy isAlphaNum
    _ <- string " -- "
    ns <- parseDotEdgeLine
    return $ (G.UserId (pack n)):ns
  ) <|>
  -- Ending edge
  ( try $ do
    n <- some $ satisfy isAlphaNum
    _ <- char ';' >> skipEOL
    return $ (G.UserId (pack n)):[]
  )

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
  parseSuccessCase parseDotEdgeLine "a -- b -- c;"
    [G.UserId (pack "a"), G.UserId (pack "b"), G.UserId (pack "c")]

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

