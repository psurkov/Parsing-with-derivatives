module Main where

import System.IO
import System.Environment

import PrologParser
import Text.Parsec

-- runParser :: String -> IO ()
-- runParser str =
--   case parseString str of
--     Left err -> print err
--     Right r -> print r

parseFromFile :: Show a => FilePath -> Parsec String () a -> IO ()
parseFromFile path parser = do
  input <- readFile path
  case parse parser "" input of
    Left err -> print err
    Right r -> do
      writeFile (path ++ ".out") (show r)


main :: IO ()
main = do
  args <- getArgs
  parseFromFile (args !! 1) prog