-- Build with: stack ghc -- -Wall ../Chapter\ 9/Cipher.hs fileIOvigenere.hs
-- Run with: ./fileIOvigenere "hello" -e

import Cipher

import System.Environment
import System.Exit
import System.IO

newtype Key = Key String deriving Show
data Mode = Encode | Decode deriving Show
data Args = Args Key Mode deriving Show

data ArgError = ArgError String deriving Show

getVigArgs :: [String] -> Either ArgError Args
getVigArgs (k:m:[]) =
  case m of
    "-d"  -> Right $ Args (Key k) Decode
    "-e"  -> Right $ Args (Key k) Encode
    x     -> (Left . ArgError) $ "Require '-d' or '-e' for modes in the second argument. " ++ x ++ " is not valid."
getVigArgs _ = Left $ ArgError "Require two arguments.\n1: Cipher key\n2: Mode decode(-d) or encode (-e)"

-- Just accept a line to make things simple
inputLoop :: IO String
inputLoop = do
  -- GHC bug right now: https://ghc.haskell.org/trac/ghc/ticket/8684
  --w <- hWaitForInput stdin 5000
  --case w of
  --  True -> hGetLine stdin
  --  False -> die "Timeout occured after five seconds of no input."
  hGetLine stdin

main :: IO ()
main = do
  args <- getVigArgs <$> getArgs
  result <- case args of
    Left (ArgError l)  -> die l
    Right (Args (Key k) Encode) -> unVCipher <$> (vEncode (VKeyword k)) <$> (VPlain <$> inputLoop)
    Right (Args (Key k) Decode) -> unVPlain <$> (vDecode (VKeyword k)) <$> (VCipher <$> inputLoop)

  putStrLn result
