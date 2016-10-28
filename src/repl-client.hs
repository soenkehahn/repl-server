
module Main where

import           Data.String.Conversions
import           Data.Text.Lazy.IO as Text
import           System.Environment

import           ReplClient

main :: IO ()
main = do
  args <- getArgs
  result <- replClient $ Just $ cs $ unwords args
  Text.putStr result
