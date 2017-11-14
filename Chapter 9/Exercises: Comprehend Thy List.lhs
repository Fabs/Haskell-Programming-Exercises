Exercises: Comprehend Thy Lists


let mySqr = [x^2 | x <- [1..5]]

[x | x <- mySqr, rem x 2 == 0]
[4, 16]

[(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
[]

take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]
[]
