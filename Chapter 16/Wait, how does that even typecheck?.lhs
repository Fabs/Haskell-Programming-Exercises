Wait, how does that even type check?

How deos (fmap . fmap) typecheck?

(.) :: (b -> c) -> (a -> b) -> a -> c
        fmap        fmap

fmap :: Functor f => (m -> n) -> f m -> f n
fmap :: Functor g => (x -> y) -> g x -> g y

I needed some help from: https://stackoverflow.com/a/23030815/350221

(.) fmap_l fmap_r
((b -> c) -> (a -> b) -> a -> c) ((m -> n) -> f m -> f n) ((x -> y) -> g x -> g y)
                                 1st arguement            2nd argument

((m -> n) -> (f m -> f n)) -> (a -> (m -> n)) -> a -> (f m -> f n)
1st arguement applied

((g x -> g y) -> (f g x -> f g y)) -> ((x -> y) -> (g x -> g y)) -> (x -> y) -> (f g x -> f g y)
2nd argument applied

((f g x -> f g y)) -> (g x -> g y) -> (x -> y) -> (f g x -> f g y)
apply (x -> y) to (g x -> g y)

(x -> y) -> (f g x -> f g y)
apply (g x -> g y) to (f g x -> f g y)

(fmap . fmap) :: (Functor g, Functor f) => (x -> y) -> f (g x) -> f (g y)
