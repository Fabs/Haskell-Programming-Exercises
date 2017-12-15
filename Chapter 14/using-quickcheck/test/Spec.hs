module Main where
import Lib
import Test.QuickCheck
import Test.Hspec
import Data.List (sort)
import Cipher

halfIdentity :: (Fractional a) => a -> a
halfIdentity = (*2) . half 

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t) 
        go y (Just x, _) = (Just y, x >= y) 


main :: IO ()
main = do
  hspec $ do
    describe "1. - half" $
      it "half times 2 is same as input" $
        property $ (\x -> (halfIdentity x) == (x::Double))

    describe "2. - listOrdered" $
      it "something" $
        property $ (\x -> listOrdered (sort x::[Int]))

    describe "3. - addition properties" $ do
      it "associative addition" $
        property $ (\x y z -> x + (y + z) == (x + y) + (z::Integer))
      it "cummutative addition" $
        property $ (\x y -> x + y ==  y + (x::Integer))

    describe "4. - multiplication properties" $ do
      it "associative multiplication" $
        property $ (\x y z -> x * (y * z) == (x * y) * (z::Integer))
      it "cummutative multiplication" $
        property $ (\x y -> x * y == (y * (x::Integer)))

    describe "5. - quot rem div mod properties" $ do
      it "quot rem" $
        property $ (\x y -> if y == 0 then True else
                              (quot x y)*y + (rem x y) == (x::Integer))
      it "div mod" $
        property $ (\x y -> if y == 0 then True else
                              (div x y)*y + (mod x y) == (x::Integer))
    describe "6. - power NOT properties" $ do
      it "pow cummutative?" $
        property $ (\x y -> x^y == y^(x::Integer))
      it "pow associative?" $
        property $ (\x y z -> x^(y^z) == ((x::Integer)^(y::Integer))^(z::Integer))

    describe "7. - reverse list" $
      it "two reverses returns the same list" $
        property $ (\x -> (reverse . reverse) x == (x::[Integer]))

    describe "8. - definition of $" $ do
      it "(+1) $ a == (+1) a" $
        property $ (\a -> ((+1) $ a) == ((+1) (a::Integer)))

      it "((+1) . (+1)) a == (+1) ((+1) a)" $
        property $ (\a -> ((+1) . (+1)) a == ((+1) ((+1) (a::Integer))))

    describe "9. - see if folds are equal" $ do
      it "foldr (:) == (++)" $
        property $ (\x -> foldr (:) [] x == ((++) [] (x::[Integer]) ))
      it "foldr (++) [] == concat" $
        property $ (\x -> (foldr (++) [] x) == (concat (x::[String])))

    describe "10. Hm. Is that so?" $ do
      it "length (take n xs) == n" $
        property $ (\n xs -> length (take n (xs::[Integer])) == (n::Int))

    describe "11. read and show" $ do
      it "f x = (read (show x)) == x" $
        property $ (\x -> (read (show x)) == (x::Integer))

    -- Fails because of float precision changes when squaring and and finding the square root
    describe "Failure - squareIdentity " $ do
      it "squareIdentity failure" $
        property $ (\x -> ((square . sqrt) x) == (x::Double))

    describe "Idempotence" $ do
      it "1. f x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)" $
        property $ (\x -> (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord (x::String)))
      it "2. f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)" $
        property $ (\x -> (sort x == twice sort x) && (sort x == fourTimes sort (x::String)))


-- Make a Gen random generator for the datatype

-- 1. 

data Fool = Fulse | Frue deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = oneof [return Fulse, return Frue]

showArbitraries :: IO ()
showArbitraries = do
  f <- sample' (arbitrary :: Gen Fool)
  putStrLn $ "Fool: " ++ (show f)

  f' <- sample' (frequency [(2, return Fulse), (1, return Frue)])
  putStrLn $ "2/3 chance Fulse, 1/3 chance of Frue:"
  putStrLn $ show f'

-- Validating ciphers
cipherTests :: IO ()
cipherTests = do
  hspec $ do
    describe "Cipher tests" $ do
      it "(unCeaser . ceaser) s == s" $
        property $ \x s -> (unCeaser x . ceaser x) s == s
      it "(vDecode . vEncode) s == s" $
        property $ \k s -> (vDecode (VKeyword k) . vEncode (VKeyword k)) (VPlain s) == VPlain s
