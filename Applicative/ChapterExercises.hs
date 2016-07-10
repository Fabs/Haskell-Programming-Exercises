ex_17_9_1_pure  = (pure :: (a -> [] a)) 4
ex_17_9_1_ap    = ((<*>) :: [] (a -> b) -> [] a -> [] b) [(+1)] [2]
ex_17_9_2_pure  = (pure :: a -> IO a) 1
ex_17_9_2_ap    = ((<*>) :: IO (a -> b) -> IO a -> IO b) (pure (++"hello")) (pure "goog")
ex_17_9_3_pure  = (pure   :: (Monoid a) => a -> (,) a a) "hello"
ex_17_9_3_ap    = ((<*>)  :: (Monoid a) => (,) a (a -> b) -> (,) a a -> (,) a b) ("Helo",(++"lll")) (" Bye", "gogo")
ex_17_9_4_pure  = (pure :: a -> (->) e a) "->Yeah"
ex_17_9_4_ap    = ((<*>) :: (->) e (a -> b) -> (->) e a -> (->) e b)
