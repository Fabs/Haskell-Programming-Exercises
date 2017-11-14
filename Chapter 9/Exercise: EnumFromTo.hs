-- Exercise: EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool True _ = []
eftBool False False = [False]
eftBool False True = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT _  = []
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd EQ _  = []
eftOrd LT EQ = [LT, EQ]
eftOrd LT LT = [LT]
eftOrd LT GT = LT:(eftOrd EQ GT)

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x > y     = []
  | x == y    = [x]
  | otherwise = x:(eftInt (x+1) y)

-- Cheat a little on this one because I don't feel like manually
-- enumerating all the possible characters in char
-- This definition could have worked for Ord and Bool too.
eftChar :: Char -> Char -> [Char]
eftChar x y = map toEnum $ eftInt (fromEnum x) (fromEnum y)
