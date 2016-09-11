{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ReplServerSpec where

import           Control.Exception
import           Data.List
import           Data.String.Conversions
import           System.IO.Silently
import           System.Timeout
import           Test.Hspec
import           Test.Mockery.Directory

import           ProcessSpec
import           ReplServer
import           ReplClient

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

  describe "replServer & replClient" $ do
    it "triggers a new invocation of the repl action" $ do
      writeFile "file" "foo"
      let config = Config {
            replCommand = "ghci",
            replAction = "readFile \"file\""
          }
      withThread (replServer config) $ do
        output :: String <- cs <$> replClient
        output `shouldSatisfy` ("foo" `isInfixOf`)
        writeFile "file" "bar"
        output :: String <- cs <$> replClient
        output `shouldSatisfy` ("bar" `isInfixOf`)

shouldTerminate :: IO a -> IO a
shouldTerminate action = do
  result <- timeout 1000000 action
  case result of
    Nothing -> throwIO $ ErrorCall "didn't terminate"
    Just a -> return a
