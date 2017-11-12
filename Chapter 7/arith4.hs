-- arith4.hs
module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 a = read (show a)

main = do
  print (roundTrip 4)
  print (id 4)
  print (roundTripPF 4)

  print ((roundTrip2 4) :: Double)
