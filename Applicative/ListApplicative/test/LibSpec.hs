module LibSpec where

import Test.Hspec
--import Test.Hspec.QuickCheck

import Lib  ( take'
            , List(Cons, Nil)
            )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    it "take' works" $ do
      let myList = Cons 'a' (Cons 'b' Nil)
      take' 1 myList `shouldBe` Cons 'a' Nil
    it "take' with negative Int is Nil" $ do
      take' (-1) (Cons "a" Nil) `shouldBe` Nil
    it "take' with Nil List is Nil" $ do
      take' 10 (Nil::List String) `shouldBe` Nil
--    prop "ourAdd is commutative" $ \x y ->
--      ourAdd x y `shouldBe` ourAdd y x
