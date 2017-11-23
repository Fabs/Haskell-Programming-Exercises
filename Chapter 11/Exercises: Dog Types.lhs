Exercises: Dog Types

> data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

1. Type constructor
2. Doggies :: * -> *
3. Doggies String :: *
4. Husky 10 :: Num a => Doggies a
5. Husky (10 :: Integer) :: Doggies Integer
6. Mastiff "Scooby Doo" :: Doggies [Char]

> data DogueDeBordeaux a = DogueDeBordeaux a

7. DogueDeBordeaux a is both a type and data constructor.
8. DogueDeBordeaux :: a -> DogueDeBordeaux a
9. DogueDeBordeaux "doggie" :: DogueDeBordeaux [Char]
