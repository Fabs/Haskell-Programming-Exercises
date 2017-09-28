> data TisAnInteger = TisAn Integer

> instance Eq TisAnInteger where
>   (==) (TisAn a) (TisAn a') = a == a'

> data TwoIntegers = Two Integer Integer

> instance Eq TwoIntegers where
>   (==) (Two a b) (Two a' b') = (a == a') && (b == b')
