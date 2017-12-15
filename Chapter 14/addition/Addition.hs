-- Addition.hs
module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)


myMult :: (Eq a, Num a) => a -> a -> a
myMult _ 0 = 0
myMult 0 _ = 0
myMult x y = x + myMult x (y-1)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5::Int, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4::Int, 2)
    it "2 times 2 is 4" $ do
      myMult 2 2 `shouldBe` (4::Int)
    it "0 times 2 is 0" $ do
      myMult 0 2 `shouldBe` (0::Int)
    it "-2 times 2 is -4" $ do
      myMult (-2) 2 `shouldBe` ((-4)::Int)
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)


genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
