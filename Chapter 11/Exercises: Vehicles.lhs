Exercises: Vehicles

> data Price = Price Integer deriving (Eq, Show)
> data Size  = Size Integer deriving (Eq, Show)
> data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
> data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
> data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

> myCar :: Vehicle
> myCar = Car Mini (Price 14000)

> urCar :: Vehicle
> urCar = Car Mazda (Price 20000)

> clownCar :: Vehicle
> clownCar = Car Tata (Price 7000)

> doge :: Vehicle
> doge = Plane PapuAir (Size 1000)

1. myCar :: Vehicle
2.

> isCar :: Vehicle -> Bool
> isCar (Car _ _) = True
> isCar _         = False

> isPlane :: Vehicle -> Bool
> isPlane (Plane _ _) = True
> isPlane _         = False

> areCars :: [Vehicle] -> [Bool]
> areCars = map isCar

3.

> getManu :: Vehicle -> Manufacturer
> getManu (Car m _) = m
> getManu _         = error "No manufacturer data available"

4. An error will occur if a Plane data type is used.
5. See above changes
