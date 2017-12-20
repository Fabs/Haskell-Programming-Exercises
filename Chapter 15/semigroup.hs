import Test.QuickCheck ( quickCheck
                       , arbitrary
                       , Arbitrary
                       , CoArbitrary
                       , oneof
                       )
import Data.Semigroup

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  a <> _ = a

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2.

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc = Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool


-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance Semigroup (Two a b) where
  a <> _ = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two Integer Bool -> Two Integer Bool -> Two Integer Bool -> Bool

-- 4.

data Three a b c = Three a b c deriving (Eq, Show)

instance Semigroup (Three a b c) where
  a <> _ = a

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc = Three Integer Bool Float -> Three Integer Bool Float -> Three Integer Bool Float -> Bool

-- Created valid Semigroup instance for the past 4 exercises but 2, 3 and 4 were not very creative after looking at some others
-- answers for these questions. Now going to (<>) more with type contraints.

-- 5.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four w x y z) <> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc = Four (Sum Int) (Sum Integer) All Any
              -> Four (Sum Int) (Sum Integer) All Any
              -> Four (Sum Int) (Sum Integer) All Any
              -> Bool

-- 6.

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7.

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj (x || y)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8.

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst _)   <> y@(Fst _) = y
  x@(Snd _) <> _         = x
  _ <> y@(Snd _)         = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    (oneof . (map return)) [Fst a, Snd b]

type OrAssoc = (Or Any All) -> (Or Any All) -> (Or Any All) -> Bool

-- 9.

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "Combine function here"

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

-- 10.

newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show _ = "Comp function here"

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = (Comp (\x -> (f x <> g x)))

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

-- 11.

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Failure x) (Failure y) = Failure (x <> y)
  (<>) (Failure x) (Success _) = Failure x
  (<>) (Success _) (Failure y) = Failure y
  (<>) (Success x) (Success _) = Success x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    (oneof . (map return)) [Failure a, Success b]

type ValidationAssoc = Validation Any All -> Validation Any All -> Validation Any All -> Bool

main :: IO ()
main = do
  putStrLn "TrivAssoc"
  quickCheck (semigroupAssoc :: TrivAssoc)
  putStrLn "IdentityAssoc"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  putStrLn "TwoAssoc"
  quickCheck (semigroupAssoc :: TwoAssoc)
  putStrLn "ThreeAssoc"
  quickCheck (semigroupAssoc :: ThreeAssoc)
  putStrLn "FourAssoc"
  quickCheck (semigroupAssoc :: FourAssoc)
  putStrLn "BoolConjAssoc"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  putStrLn "BoolConjAssoc"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  putStrLn "OrAssoc"
  quickCheck (semigroupAssoc :: OrAssoc)
  putStrLn "CombineAssoc"
  quickCheck (
    \a b c x -> ((unCombine ((a::(Combine Any All)) <> (b <> c))) (Any (x::Bool))) == ((unCombine ((a <> b) <> c)) (Any (x::Bool)))
    )
  putStrLn "CompAssoc"
  quickCheck (
    \a b c x -> ((unComp ((a::(Comp Any)) <> (b <> c))) (Any (x::Bool))) == ((unComp ((a <> b) <> c)) (Any (x::Bool)))
    )
  putStrLn "ValidationAssoc"
  quickCheck (semigroupAssoc :: ValidationAssoc)
