newtype Reader r a = Reader { runReader :: r -> a }

-- Exercise: Ask
ask :: Reader a a
ask = Reader id
