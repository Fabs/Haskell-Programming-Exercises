import qualified SimpleQueueSingle as S
import qualified SimpleQueueDuo as D
import qualified Sequence as SEQ

-- Compile with:
-- stack ghc -- -O2 simpleQueueMain.hs simpleQueueDuo.hs sequence.hs simpleQueueSingle.hs

main :: IO ()
main = do
  D.main
  S.main
  SEQ.main

-- I think I'm doing something wrong because the pop benchmarks are too short.
-- Probably because I'm not asking for all the pop results back but only the first pushed element
