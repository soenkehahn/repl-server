
module Main where

import           Data.ByteString.Lazy as LBS

import           ReplClient

main :: IO ()
main = replClient >>= LBS.putStr
