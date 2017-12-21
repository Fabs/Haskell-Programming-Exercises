import Test.QuickCheck ( quickCheck
                       , arbitrary
                       , Arbitrary
                       , CoArbitrary
                       --, oneof
                       )
import qualified Data.Semigroup as S
import Data.Monoid

semigroupAssoc :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend _ _ = Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- 2.

newtype Identity a = Identity a deriving (Show, Eq)

instance (S.Semigroup a) => S.Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity (x S.<> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty  = Identity mempty
  mappend (Identity x) (Identity y) = Identity (x <> y)

type IdentityAssoc = Identity (Sum Int)
                  -> Identity (Sum Int)
                  -> Identity (Sum Int) -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Two a b) where
  (<>) (Two x y) (Two x' y') = Two (x S.<> x') (y S.<> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two x y) (Two x' y') = Two (x <> x') (y <> y')

type TwoAssoc = Two (Sum Int) (Sum Int)
             -> Two (Sum Int) (Sum Int)
             -> Two (Sum Int) (Sum Int)
             -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- 4.

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance S.Semigroup BoolConj where
  (<>) (BoolConj x) (BoolConj y) = (BoolConj (x && y))

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj x) (BoolConj y) = BoolConj (x && y)

type BoolConjAssoc = BoolConj
                  -> BoolConj
                  -> BoolConj
                  -> Bool

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

-- 5.

newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance S.Semigroup BoolDisj where
  (<>) (BoolDisj x) (BoolDisj y) = (BoolDisj (x || y))

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj x) (BoolDisj y) = BoolDisj (x || y)

type BoolDisjAssoc = BoolDisj
                  -> BoolDisj
                  -> BoolDisj
                  -> Bool

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

-- 6.

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "Combine a b"

instance (S.Semigroup b) => S.Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (f S.<> g)

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend (Combine f) (Combine g) = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

-- 7.

newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show _ = "Comp a"

instance (S.Semigroup a) => S.Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp (f S.<> g)

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend (Comp f) (Comp g) = Comp (f <> g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

-- 8.

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Show (Mem s a) where
  show _ = "Mem s a"

instance S.Semigroup a => S.Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) = Mem (\x -> let w = (fst (f x) S.<> fst (g x))
                                       in (w, snd $ f (snd (g x))))

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s) )
  mappend (Mem f) (Mem g) = Mem (\x -> let w = (fst (f x) <> fst (g x))
                                       in (w, snd $ f (snd (g x))))

instance (CoArbitrary a, Arbitrary a, CoArbitrary s, Arbitrary s) => Arbitrary (Mem s a) where
  arbitrary = do
    f <- arbitrary
    return $ Mem f

f' :: Mem Integer [Char]
f' = Mem $ \s -> ("hi", s + 1)

main8 :: IO ()
main8 = do
  let rmzero  = runMem mempty 0
      rmleft  = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

main :: IO ()
main = do
  putStrLn "Trivial"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  putStrLn "Identity"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Identity (Sum Int) -> Bool)

  putStrLn "Two"
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two (Sum Int) (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Two (Sum Int) (Sum Int) -> Bool)

  putStrLn "BoolConj"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  putStrLn "BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  putStrLn "Combine"
  -- Assoc property
  quickCheck (
    \a b c x -> (unCombine ((a::(Combine (Sum Int) (Sum Integer))) <> (b <> c))) x
             == ((unCombine ((a <> b) <> c)) x )
    )
  -- Left Identity
  quickCheck (\a x -> (unCombine ( mempty
                                <> (a :: Combine (Sum Int) (Sum Integer)))) x
                      == (unCombine a) x)
  -- Right Identity
  quickCheck (\a x -> (unCombine ( (a :: Combine (Sum Int) (Sum Integer)))) x
                                <> mempty
                      == (unCombine a) x)

  putStrLn "Comp"
  -- Assoc property
  quickCheck (
    \a b c x -> (unComp ((a::(Comp (Sum Integer))) <> (b <> c))) x
             == ((unComp ((a <> b) <> c)) x )
    )
  -- Left Identity
  quickCheck (\a x -> (unComp ( mempty
                                <> (a :: Comp (Sum Integer) ))) x
                      == (unComp a) x)
  -- Right Identity
  quickCheck (\a x -> (unComp ( (a :: Comp (Sum Integer)))) x
                                <> mempty
                      == (unComp a) x)

  putStrLn "Mem"
  -- Assoc property
  quickCheck (
    \a b c x -> (runMem ((a::(Mem Int (Sum Integer) )) <> (b <> c))) x
             == ((runMem ((a <> b) <> c)) x )
    )
  -- Left Identity
  quickCheck (\a x -> (runMem ( mempty
                                <> (a :: Mem Int (Sum Integer) ))) x
                      == (runMem a) x)
  -- Right Identity
  quickCheck (\a x -> (runMem ( (a :: Mem Int (Sum Integer) )
                                <> mempty)) x
                      == (runMem a) x)
