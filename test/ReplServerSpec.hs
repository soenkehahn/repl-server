{-# LANGUAGE OverloadedStrings #-}

module ReplServerSpec where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.List
import           System.IO
import           System.IO.Silently
import           System.Posix.Process
import           System.Posix.Signals
import           System.Process
import           System.Timeout
import           Test.Hspec
import           Test.Mockery.Directory

import           ProcessSpec
import           ReplServer

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "replServer" $ do
    it "runs the specified command" $ do
      let config = Config {
            replCommand = "touch file",
            replAction = ""
          }
      shouldTerminate $ withThread (replServer config) $ do
        waitForFile

    it "pipes the given repl action into the command" $ do
      let config = Config {
            replCommand = "ghci",
            replAction = "writeFile \"file\" \"bla\""
          }
      shouldTerminate $ withThread (replServer config) $ do
        waitForFile

    it "outputs the repl output to stdout" $ do
      let config = Config {
            replCommand = "ghci",
            replAction = "23 + 42\nwriteFile \"file\" \"\""
          }
      output <- capture_ $ shouldTerminate $ withThread (replServer config) $ do
        waitForFile
      output `shouldSatisfy` ("65" `isInfixOf`)

shouldTerminate :: IO a -> IO a
shouldTerminate action = do
  result <- timeout (10 ^ 6) action
  case result of
    Nothing -> throwIO $ ErrorCall "didn't terminate"
    Just a -> return a
