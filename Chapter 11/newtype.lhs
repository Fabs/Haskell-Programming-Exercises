newtype section

> class TooMany a where
>   tooMany :: a -> Bool

> instance TooMany Int where
>   tooMany n = n > 42

> newtype Goats = Goats Int deriving Show

> instance TooMany Goats where
>   tooMany (Goats n) = n > 43

The second instance logic is slightly different because one checks for n > 42 and the other checks for n > 43.

Other than that, the Int doesn't need to be specified for the newtype instance.

The type of tooMany is:

λ :t tooMany
tooMany :: TooMany a => a -> Bool
