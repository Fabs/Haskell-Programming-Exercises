module SimpleQueueDuo where

import Criterion.Main

-- From Okasaki's Purely
-- Functional Data Structures
-- See: https://stackoverflow.com/a/1740603/350221
-- for explination as to how the alogrithm works
data Queue a = Queue { enqueue :: [a]
                     , dequeue :: [a]
                     } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push a (Queue e d) = (Queue (a:e) d)

pop :: Queue a -> Maybe (a, Queue a)
-- queue is empty
pop (Queue [] [])  = Nothing
-- dequeue needs to be refreshed
pop (Queue e [])   = pop (Queue [] (reverse e))
pop (Queue e (d:ds)) = Just (d, Queue e ds)

-- Pops many discarding all but the last popped
popManyDiscard :: Int -> Queue a -> Maybe (a, Queue a)
popManyDiscard 1 q = pop q
popManyDiscard i q
  | i > 1     = popManyDiscard (i-1) q
  | otherwise = Nothing 

toList :: Queue a -> [a]
toList (Queue e d) = e ++ (reverse d)

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
empty = Queue [] []

main :: IO ()
main = do
  putStrLn "BenchMark SimpleQueue with two lists"
  defaultMain [ bench "push queue 123456" $ whnf pushQueue 123456
              , bench "push n' pop queue 123456" $ whnf popQueue 123456
              ]
