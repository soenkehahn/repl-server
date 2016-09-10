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

import           ReplServer

spec :: Spec
spec = do
  describe "run" $ do
    it "runs the specified command" $ do
      let config = Config {
            replCommand = "echo foo",
            replAction = ""
          }
      readRun (run config) `shouldReturn` "foo\n"

    it "^C triggers killing of the repl process" $ do
      let config = Config {
            replCommand = "ghci",
            replAction = "()"
          }
      runProcess <- forkProcess $ do
        void $ silence $ run config stdout
      forkIO $ do
        threadDelay 100000
        signalProcess sigINT runProcess
      getProcessStatus True False runProcess
      return ()

    it "pipes the given repl action into the command" $ do
      let config = Config {
            replCommand = "ghci",
            replAction = "23 + 42\n"
          }
      shouldTerminate $ do
        withRunHandle (run config) $ \ outHandle -> do
          waitFor "65" outHandle

readRun :: (Handle -> IO ()) -> IO String
readRun run = do
  (readH, writeH) <- createPipe
  contents <- hGetContents readH
  run writeH
  return contents

withRunHandle :: (Handle -> IO ()) -> (Handle -> IO a) -> IO a
withRunHandle run test = do
  (readH, writeH) <- createPipe
  forkIO $ run writeH
  test readH

waitFor :: String -> Handle -> IO ()
waitFor needle handle = do
  line <- hGetLine handle
  if needle `isInfixOf` line
    then return ()
    else waitFor needle handle

shouldTerminate :: IO a -> IO a
shouldTerminate action = do
  result <- timeout (10 ^ 6) action
  case result of
    Nothing -> throwIO $ ErrorCall "didn't terminate"
    Just a -> return a
