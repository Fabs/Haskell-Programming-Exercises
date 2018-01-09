-- Chapter Exercises

module ReaderPractice where

-- Got a little help getting started with lookup definition and hiding lookup from:
-- https://github.com/Tclv/HaskellBook/blob/master/ch22/WarmUp.hs

--import Prelude hiding (lookup)
--import Control.Applicative
import Data.Maybe

x, y, z :: [Integer]
x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

-- The recursive definition is simple enough, don't bother with fold
--lookup :: Eq a => a -> [(a,b)] -> Maybe b
--lookup _ [] = Nothing
--lookup e ((a,b):ls)
--  | e == a    = Just b
--  | otherwise = lookup e ls

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

--uncurry :: (a -> b -> c) -> (a, b) -> c
-- that first argument is a function
-- in this case, we want it to be addition
-- summed is uncurry with addition as
-- the first argument

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8)

main :: IO ()
main = do
  print $
    sequenceA [Just (3::Int), Just 2, Just 1]
  print $ sequenceA [x,y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  putStrLn ""
  print $ sequenceA [(>3), (<8), even] (7::Int)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

myMain :: IO ()
myMain = do
  putStrLn "My turn:"
  putStrLn "1. Fold the boolean conjunction operator over the list of results of sequA (applied to some value):"
  print $ (foldr (&&) True) <$> sequA <$> (x ++ y ++ z)

  putStrLn "\n2. apply sequA to s'; you'll need fromMaybe:"
  print $ sequA $ fromMaybe 0 s'

  putStrLn "\n3. apply bolt to ys; you'll need fromMaybe:"
  print $ bolt $ fromMaybe 0 ys
