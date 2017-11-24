Exercises: How Does Your Garden Grow?

1.

> data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show
> type Gardener = String

> data Garden = Garden Gardener FlowerType deriving Show

Normal form is (I think):

data Garden = Gardener Gardenia | Gardener Daisy | Gardener Rose | Gardener Lilac deriving Show
