-- Phone Exercises

import Data.Char
import Data.List

newtype DaPhone = DaPhone [Button] deriving Show
data Button = Button Char [Char] deriving Show

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

myPhone :: DaPhone
myPhone = DaPhone
            [ Button '1' []
            , Button '2' "ABC"
            , Button '3' "DEF"
            , Button '4' "GHI"
            , Button '5' "JKL"
            , Button '6' "MNO"
            , Button '7' "PQRS"
            , Button '8' "TUV"
            , Button '9' "WXYZ"
            , Button '*' "^"
            , Button '0' " +_"
            , Button '#' ".,"
            ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone char = case mb of
  Nothing -> []
  Just b@(Button d _) -> case getButtonCount b char of
    Nothing -> []
    Just c  -> caseTapPrefix ++ [(d,c)]
  where
    mb = getButton phone char
    caseTapPrefix = if isUpper char then [('*', 1)] else []

getButton :: DaPhone -> Char -> Maybe Button
getButton (DaPhone buttons) c = m $ filter
  (\(Button d l) -> elemCaseInsensitive c l || d == c) buttons
  where m (b:_) = Just b
        m _     = Nothing

-- Get the number characters to element or Nothing
getButtonCount :: Button -> Char -> Maybe Int
getButtonCount (Button d l) c
  | d == c = Just (1 + length l)
  | otherwise =
  if elemCaseInsensitive c l
  then Just $ fst $ foldr (\x (acc, stop) ->
                      if ((toLower c) == x) || ((toUpper c) == x)
                        then if stop then (acc, True) else (acc+1, True)
                        else if stop
                             then (acc, True)
                             else (acc+1, False)) (0,False) (reverse l)
  else Nothing

-- Case insensitive elem check for Char
elemCaseInsensitive :: Char -> [Char] -> Bool
elemCaseInsensitive c l = elem (toLower c) l || elem (toUpper c) l

-- assuming the default phone definition
-- 'a' -> [('2', '1')]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead p = concat . map (reverseTaps p)

-- 2.
answer2 :: [(String, [(Digit, Presses)])]
answer2 = map (\x -> (x, cellPhonesDead myPhone x)) convo

-- 3.
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, x) acc -> x + acc) 0

answer3 :: [(String, Presses)]
answer3 = map (\(x, taps) -> (x, fingerTaps taps)) answer2

-- 4.
mostPopularLetter :: String -> Char
mostPopularLetter []    = undefined
mostPopularLetter str@(s:ss) = case isLetter s of
  False -> mostPopularLetter ss
  True  -> fst $ foldr (\c (ap, ac) -> if isLetter c && countChar str c > ac
                                       then (c, countChar str c)
                                       else (ap, ac)) (s,1) ss
  where
    countChar :: String -> Char -> Int
    countChar l c = foldr (\x acc -> if x == c then acc+1 else acc) 0 l

answer4 :: [(String, Char, Presses)]
answer4 = map (\x -> ( x
                     , mostPopularLetter x
                     , fingerTaps $ reverseTaps myPhone $ mostPopularLetter x)
                     ) convo

-- 5.
coolestLtr :: [String] -> Char
coolestLtr [] = undefined
coolestLtr s = mostPopularLetter (concat s)

coolestWord :: [String] -> String
coolestWord s  = go $ (map stripEndingPunctuation . words . concat . intersperse " ") s
  where
    go []     = ""
    go allWords@(w:ws) = fst $ foldr (\x (aw, ac) -> if countWord x allWords > ac
                                                     then (x, countWord x allWords)
                                                     else (aw, ac)) (w,1) ws
    countWord w ws = (length . filter (w==)) ws

-- Strip out ending punctuation from words to get a better count on the coolest word
stripEndingPunctuation :: String -> String
stripEndingPunctuation [] = []
stripEndingPunctuation word = reverse $ go $ reverse word
  where go [] = []
        go str@(s:ss)
          | isPunctuation s = ss
          | otherwise       = str

answer5_coolestLtr :: Char
answer5_coolestLtr = coolestLtr convo
answer5_coolestWord :: String
answer5_coolestWord = coolestWord convo
