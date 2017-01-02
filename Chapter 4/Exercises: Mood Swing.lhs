> data Mood = Blah | Woot deriving Show

1. The type constructor is `Mood`
2. The values are `Blah` and `Woot`
3. The type signature cannot include a value like `Woot`. The type has to be `Mood`. The correct type signature is as follows:

> changeMood :: Mood -> Mood

4. `changeMood` implementation below.

> changeMood Blah = Woot
> changeMood Woot = Blah

5. This file.
