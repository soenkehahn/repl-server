{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ReplServerSpec where

import           Control.Concurrent
import           Control.Exception
import           Data.List
import           Data.String.Conversions
import           System.Environment
import           System.IO
import           System.IO.Silently
import           System.Process
import           System.Timeout
import           Test.Hspec
import           Test.Mockery.Directory
import           Test.Mockery.Environment

import           ProcessSpec
import           ReplClient
import           ReplServer

setTestPrompt :: IO () -> IO ()
setTestPrompt action = do
  writeFile ".ghci" ":set prompt \"==> \"\n"
  callCommand "chmod go-w .ghci ."
  env <- getEnvironment
  withEnvironment (env ++ [("HOME", ".")]) $ do
    action

spec :: Spec
spec = around_ inTempDirectory $ around_ setTestPrompt $ do
  describe "replServer" $ do
    it "runs the specified command" $ do
      let config = Config {
            replCommand = "touch file",
            replAction = "",
            replPrompt = "==> "
          }
      shouldTerminate $ withThread (replServer config) $ do
        waitForFile "file"

    it "pipes the given repl action into the command" $ do
      let config = Config {
            replCommand = "ghci",
            replAction = "writeFile \"file\" \"bla\"",
            replPrompt = "==> "
          }
      shouldTerminate $ withThread (replServer config) $ do
        waitForFile "file"

    it "outputs the repl output to stdout" $ do
      let config = Config {
            replCommand = "ghci",
            replAction = "23 + 42",
            replPrompt = "==> "
          }
      output <- capture_ $ shouldTerminate $ withThread (replServer config) $ do
        threadDelay 300000
      output `shouldSatisfy` ("65" `isInfixOf`)

  describe "replServer & replClient" $ do
    it "triggers a new invocation of the repl action" $ shouldTerminate $ do
      writeFile "file" "foo"
      let config = Config {
            replCommand = "ghci",
            replAction = "readFile \"file\"",
            replPrompt = "==> "
          }
      silence $ withThread (replServer config) $ do
        waitForFile ".repl-server.socket"
        output :: String <- cs <$> replClient
        output `shouldSatisfy` ("foo" `isInfixOf`)
        writeFile "file" "bar"
        output :: String <- cs <$> replClient
        output `shouldSatisfy` ("bar" `isInfixOf`)

    context "when a command writes to stdout" $ do
      it "relays the lines exactly" $ do
        let config = Config {
              replCommand = "ghci",
              replAction = "putStrLn \"boo\"",
              replPrompt = "==> "
            }
        silence $ withThread (replServer config) $ do
          waitForFile ".repl-server.socket"
          output :: String <- cs <$> replClient
          lines output `shouldContain` ["boo"]

    it "relays stderr" $ do
      let config = Config {
            replCommand = "ghci",
            replAction = "True && ()",
            replPrompt = "==> "
          }
      hSilence [stderr] $ withThread (replServer config) $ do
        waitForFile ".repl-server.socket"
        output :: String <- cs <$> replClient
        output `shouldSatisfy` ("Couldn't match expected type ‘Bool’ with actual type ‘()’" `isInfixOf`)

shouldTerminate :: IO a -> IO a
shouldTerminate action = do
  result <- timeout 1000000 action
  case result of
    Nothing -> throwIO $ ErrorCall "didn't terminate"
    Just a -> return a
