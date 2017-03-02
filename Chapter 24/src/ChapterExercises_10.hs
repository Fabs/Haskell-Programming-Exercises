{-# OPTIONS_GHC -Wall -Werror #-}

module ChapterExercises_10 where

--import Control.Applicative
import Text.Trifecta
import Data.Text

--import Test.Hspec
--import Test.HUnit

import ParseTestCaseHelpers

-- Taken from https://github.com/NorfairKing/haphviz/blob/master/src/Text/Dot/Types/Internal.hs#L64
-- | Haphviz internal graph content AST
data Dot = Node NodeId [Attribute]
         | Edge NodeId NodeId [Attribute]
         | Declaration DecType [Attribute]
         | Ranksame Dot
         | Subgraph Text Dot
         | RawDot Text
         | Label Text
         | Rankdir RankdirType
         | DotSeq Dot Dot
         | DotEmpty
    deriving (Show, Eq)

-- | Attribute name: just text
type AttributeName = Text

-- | Attribute value: just text
type AttributeValue = Text

-- | Attribute: a tuple of name and value.
type Attribute = (AttributeName, AttributeValue)

-- | A node identifier.
--
-- This is either a user supplied name or a generated numerical identifier.
data NodeId = UserId Text
            | Nameless Int
    deriving (Show, Eq)

-- | Declaration type
--
-- Used to declare common attributes for nodes or edges.
data DecType = DecGraph
             | DecNode
             | DecEdge
    deriving (Show, Eq)

-- | Rankdir Type
--
-- Used to specify the default node layout direction
data RankdirType = LR
                 | RL
                 | TB
                 | BT
    deriving (Show, Eq)


parseDot :: Parser Dot
parseDot = undefined

-- Run a series of simple parsing tests
-- with fixed inputs and fixed expected results
parserTests :: IO ()
parserTests = do
  putStrLn "\nTesting Dot parser:"

  parseSuccessCase parseDot "fefe" DotEmpty
  --parseFailCase parseIPv6Group "11111"

  --parseSuccessCase parseIPv6Sections "0000:001" $ [Group 0, Group 1]
  --parseSuccessCase parseIPv6Sections "0000:1:001" $
  --  [Group 0, Group 1, Group 1]
  --parseSuccessCase parseIPv6Sections "0000:1::001" $
  --  [Group 0, Group 1, Collapse, Group 1]
  --parseSuccessCase parseIPv6Sections "0000:1::001" $
  --  [Group 0, Group 1, Collapse, Group 1]
  --parseFailCase parseIPv6Sections "1:1::1::11"
