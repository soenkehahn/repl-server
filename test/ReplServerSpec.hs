{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ReplServerSpec where

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
spec = around_ (inTempDirectory . setTestPrompt. shouldTerminate . silence. hSilence [stderr]) $ do
  describe "replServer" $ do
    it "runs the specified command" $ do
      let config = Config {
            replCommand = "touch file",
            replPrompt = "==> "
          }
      withThread (replServer config) $ do
        waitForFile "file"

  describe "replServer & replClient" $ do
    it "pipes the given repl action into the command" $ do
      withReplSocket $ do
        _ <- replClient "writeFile \"file\" \"bla\""
        waitForFile "file"

    it "outputs the repl output to stdout" $ do
      output <- capture_ $ withReplSocket $ do
        _ <- replClient "writeFile \"file\" \"bla\" >> print (23 + 42)"
        waitForFile "file"
      output `shouldSatisfy` ("65" `isInfixOf`)

    it "triggers a new invocation of the repl action" $ do
      writeFile "file" "foo"
      silence $ withReplSocket $ do
        output :: String <- cs <$> replClient "readFile \"file\""
        output `shouldSatisfy` ("foo" `isInfixOf`)
        writeFile "file" "bar"
        output :: String <- cs <$> replClient "readFile \"file\""
        output `shouldSatisfy` ("bar" `isInfixOf`)

    context "when a command writes to stdout" $ do
      it "relays the lines exactly" $ do
        silence $ withReplSocket $ do
          output :: String <- cs <$> replClient "putStrLn \"boo\""
          lines output `shouldContain` ["boo"]

    it "relays stderr" $ do
      hSilence [stderr] $ withReplSocket $ do
        output :: String <- cs <$> replClient "True && ()"
        output `shouldSatisfy` ("Couldn't match expected type ‘Bool’ with actual type ‘()’" `isInfixOf`)

    it "allows to overwrite the repl action" $ do
      silence $ withReplSocket $ do
        output :: String <- cs <$> replClient "putStrLn \"bar\""
        output `shouldContain` "bar"

shouldTerminate :: IO a -> IO a
shouldTerminate action = do
  result <- timeout 1000000 action
  case result of
    Nothing -> throwIO $ ErrorCall "didn't terminate"
    Just a -> return a

withReplSocket :: IO a -> IO a
withReplSocket action = withThread (replServer config) $ do
  waitForFile ".repl-server.socket"
  action
  where
    config = Config {
      replCommand = "ghci",
      replPrompt = "==> "
    }
