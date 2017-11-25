Exercises: The Quad

> data Quad = One | Two | Three | Four deriving (Eq, Show)

1. eQuad :: Either Quad Quad

eQuad Can be 8 different possiblities


2. prodQuad :: (Quad, Quad)

prodQuad can be 4*4 = 16 different possibilities

3. funcQuad :: Quad -> Quad

funcQuad can be 4*4 = 16 different possiblities

4. prodTBool :: (Bool, Bool, Bool)

prodTBool can be 2^3 = 8 different possibilities

5. gTwo :: Bool -> Bool -> Bool

gTwo can have 2^3 = 8 different possibilities

6. fTwo :: Bool -> Quad -> Quad

fTwo can have 2 * 4 * 4 = 32 different possibilities
