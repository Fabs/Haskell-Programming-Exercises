module LogicGoats where

-- 1. 
class TooMany a where
  tooMany :: a -> Bool

newtype SomethingNew = SomethingNew (Int, String)

instance TooMany SomethingNew where
  tooMany (SomethingNew (n,_)) = n > 42

exercise1 :: Int -> IO ()
exercise1 i
  | tooMany (SomethingNew (i, "My new things")) == True = putStrLn "Too many here"
  | otherwise                                           = putStrLn "Not too many here"

-- 2.
newtype SomethingNew2 = SomethingNew2 (Int, Int)
newtype NumGoats = NumGoats Int

instance TooMany SomethingNew2 where
  tooMany (SomethingNew2 (x,y)) = (x + y) > 42

exercise2 :: NumGoats -> NumGoats -> IO ()
exercise2 (NumGoats x) (NumGoats y)
  | tooMany (SomethingNew2 (x, y)) == True = putStrLn "Too many here"
  | otherwise                              = putStrLn "Not too many here"

-- 3.
instance TooMany Int where
  tooMany n = n > 42

newtype SomethingNew3 a = SomethingNew3 (a, a)

instance (Num a, TooMany a) => TooMany (SomethingNew3 a) where
  tooMany (SomethingNew3 (x, y) ) = (tooMany (x + y))

exercise3 :: (Int, Int) -> IO ()
exercise3 (x, y)
  | tooMany (SomethingNew3 (x, y)) == True = putStrLn "Too many here"
  | otherwise                              = putStrLn "Not too many here"

