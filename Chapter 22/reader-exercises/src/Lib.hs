{-# LANGUAGE InstanceSigs #-}

module Lib where

-- Exercise: Reading Comprehension

--import Test.QuickCheck
--import Test.QuickCheck.Checkers
--import Test.QuickCheck.Classes

-- 1.
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

-- 2.
newtype Reader r a = Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks = Reader

-- 3.

-- I don't know how to test Functor with QuickCheck checkers because I
-- don't know how to meaningfully create a Eq instance.
-- Commented out QuickCheck code.

-- instance Show (Reader r a) where
--   show _ = "Reader here!@"

-- instance Eq (Reader r a) where
--   eq (Reader ra) (Reader ra') = ???

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rab) (Reader ra) = Reader $ rab <*> ra

--instance (CoArbitrary r, Arbitrary a) => Arbitrary (Reader r a) where
--  arbitrary = do
--    r_to_a <- arbitrary
--    return $ Reader r_to_a

-- Required for checkers
-- instance (Eq a) => EqProp (Reader r a) where (=-=) = eq

--test_Reader :: IO ()
--test_Reader = do
--  putStrLn "Reader"
--  quickBatch (functor (undefined :: Reader Int (Int, Int, [Int])))

-- Exercise: Reader Monad

-- 1.

-- Got a hint here: https://github.com/Tclv/HaskellBook/blob/master/ch22/ShortExercise.hs#L48
-- `ra` needs `r` applied to become `a`.
-- Then `aRb` can have `a` applied which becomes `Reader r -> a`.
-- Using runReader we get just `r -> a` which can then have `r` applied once again
-- leaving only `a` which now finally gives `Reader r -> b` for the (>>=) function.
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) aRb = Reader $ \r -> runReader (aRb (ra r)) r

-- 2.
newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName
                     , dogName :: DogName
                     , address :: Address
                     } deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName
               , dogsAddress :: Address
               } deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy
