{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr
import Test.QuickCheck
import Test.QuickCheck.Function

{- Chapter exercises

1. -} 

data Bool' = False | True

-- A Functor can NOT be written for Bool because it is of kind *

-- 2.

data BoolAndSomethingElse a = False' a | True' a

-- Yes a Functor can be written for 'BoolAndSomethingElse a' because it is of kind * -> *

-- 3.

data BoolAndMaybeSomethingElse a = Falsish | Truiish a

-- Yes a Functor can be written for 'BoolAndMaybeSomethingElse a' because it is of kind * -> *

-- 4.

newtype Mu f = InF { outF :: f (Mu f) }

--No, a Functor cannot be written for 'Mu f' because it is of kind (* -> *) -> *

--instance Functor Mu where
--  fmap f (InF fa) = undefined

--Chapter exercises.hs:27:18: error:
--    • Expected kind ‘* -> *’, but ‘Mu’ has kind ‘(* -> *) -> *’
--    • In the first argument of ‘Functor’, namely ‘Mu’
--      In the instance declaration for ‘Functor Mu’
--   |
--27 | instance Functor Mu where
--   |

-- 5.

data D = D (Array Word Word) Int Int

-- No, a Functor cannot be written for 'D' because it has kind *

-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.

-- 1. data Sum a b = First a | Second b

data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

-- 2. data Company a b c = DeepBlue a c | Something b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3. data More a b = L a b a | R b a b deriving (Eq, Show)

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instance for the following datatypes.

-- Functor laws (Copied from earlier exercise)
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
-- 1.

data Quant a b = Finance | Desk a | Bloor b deriving (Show, Eq)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    (oneof . (fmap return)) [Finance, Desk a, Bloor b]

type QuantFC = Quant Char Int -> IntToInt -> IntToInt -> Bool

test_Quant :: IO ()
test_Quant = do
  putStrLn "Quant:"
  quickCheck (functorIdentity :: (Quant Float Int -> Bool))
  quickCheck (functorCompose' :: QuantFC)

-- 2.

data K a b = K a deriving (Show, Eq)

instance Functor (K a) where fmap _ (K a) = K a

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return $ K a

type KFC = K Float Int -> IntToInt -> IntToInt -> Bool

test_K :: IO ()
test_K = do
  putStrLn "K:"
  quickCheck (functorIdentity :: (K Float Int -> Bool))
  quickCheck (functorCompose' :: KFC)

-- 3.

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) =  (Flip (K' (f a)))

instance (Arbitrary b) => Arbitrary (Flip K' a b) where
  arbitrary = do
    b <- arbitrary
    return $ Flip $ K' b

type FlipFC = Flip K' Float Int -> IntToInt -> IntToInt -> Bool

test_FlipFC :: IO ()
test_FlipFC = do
  putStrLn "Flip:"
  quickCheck (functorIdentity :: (Flip K' Float Int -> Bool))
  quickCheck (functorCompose' :: FlipFC)

-- 4.

data EvilGoateeConst a b = GoatyConst b deriving (Show, Eq)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    return $ GoatyConst b

type EvilGoateeConstFC = EvilGoateeConst Float Int -> IntToInt -> IntToInt -> Bool

test_EvilGoateeConst :: IO ()
test_EvilGoateeConst = do
  putStrLn "EvilGoateeConst:"
  quickCheck (functorIdentity :: (EvilGoateeConst Float Int -> Bool))
  quickCheck (functorCompose' :: EvilGoateeConstFC)

-- 5.

data LiftItOut f a = LiftItOut (f a) deriving (Show, Eq)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

instance (Arbitrary a) => Arbitrary (LiftItOut Maybe a) where
  arbitrary = do
    a <- arbitrary
    (oneof . (fmap return)) [LiftItOut Nothing, LiftItOut $ Just a]

type LiftItOutFC = LiftItOut Maybe Int -> IntToInt -> IntToInt -> Bool

test_LiftItOut :: IO ()
test_LiftItOut = do
  putStrLn "LiftItOut:"
  quickCheck (functorIdentity :: (LiftItOut Maybe Int -> Bool))
  quickCheck (functorCompose' :: LiftItOutFC)

-- 6.

data Parappa f g a = DaWrappa (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

instance (Arbitrary a) => Arbitrary (Parappa Maybe [] a) where
  arbitrary = do
    a <- arbitrary
    (oneof . (fmap return)) [ DaWrappa x y | x <- [Nothing, Just a], y <- [[], [a]]]

type ParappaFC = Parappa Maybe [] Int -> IntToInt -> IntToInt -> Bool

test_Parappa :: IO ()
test_Parappa = do
  putStrLn "Parappa:"
  quickCheck (functorIdentity :: (Parappa Maybe [] Int -> Bool))
  quickCheck (functorCompose' :: ParappaFC)

-- 7.

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Show, Eq)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne Maybe [] a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    (oneof . (fmap return)) [ IgnoringSomething x y | x <- [Nothing, Just a], y <- [[], [b]]]

type IgnoreOneFC = IgnoreOne Maybe [] Float Int -> IntToInt -> IntToInt -> Bool

test_IgnoreOne :: IO ()
test_IgnoreOne = do
  putStrLn "IgnoreOne:"
  quickCheck (functorIdentity :: (IgnoreOne Maybe [] Float Int -> Bool))
  quickCheck (functorCompose' :: IgnoreOneFC)

-- 8.

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Show, Eq)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Notorious Maybe a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    (oneof . (fmap return)) [ Notorious x y z | x <- [Nothing, Just a], y <- [Nothing, Just b], z <- [Nothing, Just c]]

type NotoriousFC = Notorious Maybe Char Float Int -> IntToInt -> IntToInt -> Bool

test_Notorious :: IO ()
test_Notorious = do
  putStrLn "Notorious:"
  quickCheck (functorIdentity :: (Notorious Maybe Char Float Int -> Bool))
  quickCheck (functorCompose' :: NotoriousFC)

-- 9.

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    as <- arbitrary
    (oneof . (fmap return)) [ Nil, Cons a as ]

type ListFC = List Int -> IntToInt -> IntToInt -> Bool

test_List :: IO ()
test_List = do
  putStrLn "List:"
  quickCheck (functorIdentity :: (List Int -> Bool))
  quickCheck (functorCompose' :: ListFC)

-- 10.

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  -- A VERITABLE HYTDRA OF GOATS
  deriving (Show, Eq)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = do
    a <- arbitrary
    lord1 <- arbitrary
    lord2 <- arbitrary
    lord3 <- arbitrary
    (oneof . (fmap return)) [ NoGoat, OneGoat a, MoreGoats lord1 lord2 lord3 ]

type GoatLordFC = GoatLord Int -> IntToInt -> IntToInt -> Bool

test_GoatLord :: IO ()
test_GoatLord = do
  putStrLn "GoatLord:"
  quickCheck (functorIdentity :: (GoatLord Int -> Bool))
  quickCheck (functorCompose' :: GoatLordFC)

-- 11.

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = (Print s (f a))
  fmap f (Read s_to_a) = (Read (fmap f s_to_a))

instance (Show a) => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print s a) = "Print " ++ s ++ (show a)
  show (Read _)    = "Read some function"

instance (Eq a) => Eq (TalkToMe a) where
  (==) Halt Halt                  = Prelude.True
  (==) (Print s a) (Print s' a')  = (s == s') && (a == a')
  (==) (Read _) (Read _)          = Prelude.True -- Don't know so just assume it is true for QuickChecks
  (==) _ _                        = Prelude.False

instance (Arbitrary a) => Arbitrary (TalkToMe a) where
  arbitrary = do
    a <- arbitrary
    s <- arbitrary
    s_to_a <- arbitrary
    (oneof . (fmap return)) [ Halt, Print s a, Read s_to_a]

type TalkToMeFC = TalkToMe Int -> IntToInt -> IntToInt -> Bool

test_TalkToMe :: IO ()
test_TalkToMe = do
  putStrLn "TalkToMe:"
  quickCheck (functorIdentity :: (TalkToMe Int -> Bool))
  quickCheck (functorCompose' :: TalkToMeFC)
