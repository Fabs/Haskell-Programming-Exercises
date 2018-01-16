-- Morra: Guess the total number of fingers of all players



-- TODO: Replace `read` because it likely will throw:
-- *** Exception: Prelude.read: no parse

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Trans.State
import Control.Monad.Trans
--import Control.Monad.IO.Class
import Data.Default
import System.Random
import System.IO


-- Complete state of current Morra game
data Morra = Morra [Player] PlayHistory deriving (Show)

-- Types
newtype Score   = Score Integer deriving (Num, Show, Default, Ord, Eq)
newtype Hand    = Hand Integer deriving (Integral, Real, Ord, Eq, Enum, Num, Show, Default)
newtype Guess   = Guess Integer deriving (Num, Show, Default)
newtype Name    = Name String deriving (Show)
data PlayerType = Human | AI deriving (Show)
data Player     = Player Name Score PlayerType deriving (Show)
data Play       = Play Name Hand Guess deriving (Show)
newtype Plays   = Plays [Play] deriving (Show, Default)
newtype PlayHistory = PlayHistory [Plays] deriving (Show, Default)

-- Default game state (Zero players)
instance Default Morra where def = Morra [] def

-- Main - CPU vs Human
main :: IO ()
main = do
  putStrLn "-- Morra --"
  initState <- execStateT morraVsCPU_setup def
  endState  <- execStateT morraLoop initState

  putStrLn "\nDumping game state:"
  print endState

-- CPU play only
aiBattle :: IO ()
aiBattle = do
  putStrLn "-- Morra (AI battle) --"
  initState <- execStateT morraAIonly_setup def
  endState  <- execStateT morraLoop initState

  putStrLn "\nDumping game state:"
  print endState

-- Human play only
humanBattle :: IO ()
humanBattle = do
  putStrLn "-- Morra (Human battle) --"
  initState <- execStateT morraHumanOnlySetup def
  endState  <- execStateT morraLoop initState

  putStrLn "\nDumping game state:"
  print endState


-- Game loop
morraLoop :: StateT Morra IO ()
morraLoop = do
  s <- (lift . execStateT morraTurn) =<< get
  if isGameOver s
  then do
    lift $ putStrLn $ "Detected a winner or tie after " ++ (show $ gameLength s) ++ " turns."
    lift $ putStrLn "Game Over."
    put s
  else (lift $ execStateT morraLoop s) >>= put

morraGetPlay :: Morra -> Player -> IO Play
morraGetPlay (Morra players _) (Player (Name name) _ Human) = do
  putStrLn $ name ++ ", how many fingers are to hold up?"
  hand <- enterNum (0, 5)
  let num_players = toInteger (length players)
  putStr $ "Out of " ++ (show num_players) ++ " players, "
  putStrLn "guess how many total fingers will be shown? "
  guess <- enterNum (0, num_players * 5)
  -- Clear screen in case other human players are watching
  putStr "\ESC[2J"
  return $ Play (Name name) (Hand hand) (Guess guess)

morraGetPlay (Morra players history) (Player (Name name) _ AI) = do
  -- Try to determine a pattern from play history
  -- Get all other players
  let opponents = filter (\(Player (Name n) _ _) -> n /= name) players
  predictions <- sequenceA $ map (predictPlay history) opponents

  -- Add up all the player predictions for the final guess
  let finalPrediction = sum predictions

  -- Choose a random play
  hand <- randomRIO (0, 5)

  -- Add my play to prediction
  let guess = hand + (toInteger finalPrediction)
  return $ Play (Name name) (Hand hand) (Guess guess)

predictPlay :: PlayHistory -> Player -> IO Hand
predictPlay (PlayHistory allPlays) (Player (Name name) _ _) = do
  -- filter out plays for player
  let ph = concat $ map (\(Plays ps) -> filter (\(Play (Name n) _ _) -> n == name) ps) allPlays

  -- Collect all the predictions
  let predictedGuesses = findPatternThreeGuesses (take 2 ph) ph

  if (length predictedGuesses > 0)
  then do
    -- Randomly select a prediction
    playPick <- (predictedGuesses !!) <$> randomRIO (0, length predictedGuesses - 1)
    -- Fuzz pick
    playPick' <- fuzzPick playPick
    return playPick'
  else randomRIO (0, 5) >>= return . Hand
  where
    findPatternThreeGuesses :: [Play] -- Current plays
                            -> [Play] -- Past plays
                            -> [Hand] -- Pattern matches
    findPatternThreeGuesses [] _       = []
    findPatternThreeGuesses (_:[]) _   = []
    findPatternThreeGuesses _ []       = []
    findPatternThreeGuesses _ (_:[])   = []
    findPatternThreeGuesses _ (_:_:[]) = []
    -- Get the last two plays
    -- See if those two plays repeat in history at all
    findPatternThreeGuesses current@((Play _ ca _):(Play _ cb _):_) ((Play _ ha _):phb@(Play _ hb _):phc@(Play _ hc _):phs)
      | ca == ha && cb == hb               = hc:findPatternThreeGuesses current (phb:phc:phs)
      | otherwise                          = findPatternThreeGuesses current (phb:phc:phs)
    fuzzPick :: Hand -> IO Hand
    fuzzPick (Hand h) = do
      fuzzFactor <- randomRIO (-1, 1)
      return $ if (fuzzFactor + h) >= 5
      then Hand 5
      else if (fuzzFactor + h) <= 0
           then Hand 0
           else Hand (fuzzFactor + h)

-- One turn of Morra
morraTurn :: StateT Morra IO ()
morraTurn = do
  
  (Morra players (PlayHistory history)) <- get
  let turn_num = length history + 1
  lift $ putStrLn $ "\nTurn #" ++ (show turn_num)

  -- Collect all of the plays from players
  plays <- lift $ traverse (morraGetPlay (Morra players (PlayHistory history))) players

  -- Call out the play
  _ <- lift $ flip mapM plays (\(Play (Name n) (Hand h) (Guess g)) -> do
      putStrLn $ (show n) ++ ": \"" ++ (show g) ++ "!\" shows " ++ (show h) ++ " fingers" 
    )

  -- Apply play
  modify (applyTurn (Plays plays))

  -- Display results of the last play
  (Morra players' _) <- get
  _ <- lift $ flip mapM players' (\(Player (Name n) (Score s) _) -> do
      putStr $ (show n) ++ ": " ++ (show s) ++ " points. "
    )
  lift $ putStrLn ""

-- Creates a Morra state with CPU and human player
morraVsCPU_setup :: StateT Morra IO ()
morraVsCPU_setup = do
  name <- lift enterName 
  modify $ addPlayer name Human
  modify $ addPlayer "CPU" AI

-- Creates a Morra state only AI players
morraAIonly_setup :: StateT Morra IO ()
morraAIonly_setup = do
  modify $ addPlayer "CPU 1" AI
  modify $ addPlayer "CPU 2" AI

-- Creates a Morra state only AI players
morraHumanOnlySetup :: StateT Morra IO ()
morraHumanOnlySetup = do
  name <- lift enterName 
  modify $ addPlayer name Human

  name2 <- lift enterName 
  modify $ addPlayer name2 Human

isGameOver :: Morra -> Bool
isGameOver (Morra ps _) = foldr (\(Player _ s _) a -> ((s >= (Score 3)) || a)) False ps

gameLength :: Morra -> Integer
gameLength (Morra _ (PlayHistory h)) = toInteger $ length h

-- Apply turn based on plays
applyTurn :: Plays -> Morra -> Morra
applyTurn plays@(Plays []) (Morra players (PlayHistory h)) = Morra players (PlayHistory (plays:h))
applyTurn (Plays ps) (Morra players (PlayHistory history)) =
  Morra (fmap (\(Player n s t) ->
                 if (isCorrectGuesser (Player n s t))
                 then Player n (s+1) t
                 else Player n s t) players)
        (PlayHistory ((Plays ps):history))
  where
    total = foldr (\(Play _ (Hand h) _) a -> h+a) 0 ps
    correctGuesses = filter (\(Play _ _ (Guess g)) -> g == total) ps
    isCorrectGuesser (Player (Name n) _ _) = any (\(Play (Name n') _ _) -> n == n') correctGuesses

addPlayer :: String -> PlayerType -> Morra -> Morra
addPlayer n t m@(Morra ps h) =
        if any (\(Player (Name n') _ _) -> n' == n) ps
        then addPlayer (n ++ "_") t m
        else Morra ((Player (Name n) def t):ps) h

enterName :: IO String
enterName = do
  putStr "Enter your name: "
  n <- getLine
  if (length n == 0)
  then enterName
  else return n

enterNum :: (Integer,Integer) -> IO Integer
enterNum (low, high) = do
  if (low > high)
  then do
    putStrLn $ "Sorry, it isn't possible to get a number between (low) "
             ++ (show low) ++ " and (high) " ++ (show high)
    putStrLn "Here, take low value back."
    return low
  else do
    hSetBuffering stdin NoBuffering  
    putStr $ "Enter number within the range " ++ (show low) ++ "-" ++ (show high) ++ ": "
    n <- getLine >>= return . read
    if (n >= low) && (n <= high)
    then return n
    else enterNum (low, high)
