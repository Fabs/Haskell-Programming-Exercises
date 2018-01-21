module SimpleQueueSingle where

import Criterion.Main

data Queue a = Queue [a] deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push a (Queue as) = (Queue (a:as))

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue []) = Nothing
pop (Queue as) = Just (last as, Queue (init as))

-- Pops many discarding all but the last popped
popManyDiscard :: Int -> Queue a -> Maybe (a, Queue a)
popManyDiscard 1 q = pop q
popManyDiscard i q
  | i > 1     = popManyDiscard (i-1) q
  | otherwise = Nothing 

toList :: Queue a -> [a]
toList (Queue as) = as

constructQueue :: Int -> Queue Int
constructQueue i = go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (push n xs)

pushQueue :: Int -> [Int]
pushQueue i = toList $ constructQueue i

popQueue :: Int -> [Int]
popQueue i = [a]
  where Just (a, _) = popManyDiscard i $ constructQueue i

empty :: Queue a
empty = Queue []

main :: IO ()
main = defaultMain
  [ bench "push queue 123456" $ whnf pushQueue 123456
  , bench "push n' pop queue 123456" $ whnf popQueue 123456
  ]
