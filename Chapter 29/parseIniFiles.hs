import System.Environment
import System.Directory
import System.Exit
import System.IO
import Data.List
--import Data.Either
import qualified Data.Map.Strict as M
import Data.ByteString.Char8 (pack)

-- Import in INI parser from chapter 24
-- from iniParser
import Text.Trifecta
import Data.Ini

data Args = Args { unArgs :: FilePath } deriving Show
data ArgError = ArgError String deriving Show

getParseArgs :: [String] -> Either ArgError Args
getParseArgs (p:[]) = Right $ Args p
getParseArgs _ = Left $ ArgError "Require one arguments.\n1: FilePath to directory"

-- Couldn't figure out how to get the base-4.10.0.0 in GHCI
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

main :: IO ()
main = do
  eArgs <- getParseArgs <$> getArgs

  case eArgs of
    Left  (ArgError s) -> die s
    _                  -> return ()

  let args = fromRight (Args "") eArgs

  inis <- filter (isSuffixOf ".ini") <$> (listDirectory $ unArgs args)

  -- Filter out all failures

  configTuple <- traverse (\filename -> do
             handle <- openFile (unArgs args ++ "/" ++ filename) ReadMode
             contents <- pack <$> hGetContents handle
             let config = parseByteString parseIni mempty contents
             return (filename, config)
           ) inis

  print $ M.fromList configTuple
