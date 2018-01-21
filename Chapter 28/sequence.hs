module Sequence where

import Criterion.Main
import Data.Sequence
import Prelude hiding (null, splitAt, length)
import Data.Foldable hiding (null, length)

newtype Queue a = Queue (Seq a) deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push a (Queue as) = Queue (a <| as)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue s)
  | null s    = Nothing
  | otherwise = let (i,l) = splitAt (length s - 1) s
                in  Just ((index l 0), Queue i)

-- Pops many discarding all but the last popped
popManyDiscard :: Int -> Queue a -> Maybe (a, Queue a)
popManyDiscard 1 q = pop q
popManyDiscard i q
  | i > 1     = popManyDiscard (i-1) q
  | otherwise = Nothing 

toList :: Queue a -> [a]
toList (Queue as) = Data.Foldable.toList as

constructQueue :: Int -> Queue Int
constructQueue i = go i Sequence.empty
  where go 0 xs = xs
        go n xs = go (n-1) (push n xs)

pushQueue :: Int -> [Int]
pushQueue i = Sequence.toList $ constructQueue i

popQueue :: Int -> [Int]
popQueue i = [a]
  where Just (a, _) = popManyDiscard i $ constructQueue i

empty :: Queue a
empty = Queue Data.Sequence.empty

main :: IO ()
main = do
  putStrLn "BenchMark SimpleQueue with Sequence (instead of lists)"
  defaultMain [ bench "push queue 123456" $ whnf pushQueue 123456
              , bench "push n' pop queue 123456" $ whnf popQueue 123456
              ]
