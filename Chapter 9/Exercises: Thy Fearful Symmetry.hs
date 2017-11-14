{--
In the final example above, why does it only return a single a?

Because the first character is an 'a'

Exercises: Thy Fearful Symmetry
--}

-- 1

myWords :: String -> [String]
myWords []     = []
myWords x = (takeWord x):(myWords $ dropWord x)
  where
    takeWord = takeWhile (/=' ')
    dropWord = (dropWhile (==' ') . dropWhile (/=' '))

-- 2. See PoemLines.hs

-- 3. See PoemLines.hs mySection function
