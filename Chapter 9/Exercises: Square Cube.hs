-- Exercises: Square Cube

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1.
q1 = [ (x, y) | x <- mySqr, y <- myCube ]

-- 2.
q2 = [ (x, y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]

-- 3.
howManyTuples = length q2
