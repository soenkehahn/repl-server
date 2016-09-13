{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ReplServer where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Data.String
import           Data.String.Conversions
import           GHC.IO.Handle
import           Network.HTTP.Types
import           Network.Socket hiding (recv)
import           Network.Wai
import           Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import           Prelude ()
import           Prelude.Compat
import           System.IO
import           WithCli

import           Handle
import           Process
import           Socket

data Config
  = Config {
    replCommand :: String,
    replAction :: String,
    replPrompt :: String
  }
  deriving (Generic, Show)

instance HasArguments Config

replServer :: Config -> IO ()
replServer config = do
  withProcess (replCommand config) $ \ (to, from, fromStdErr, _) -> do
    _ <- toNextPrompt config from
    _ <- executeReplAction config to from fromStdErr
    withApplication (app config to from fromStdErr) $ do
      forever $ threadDelay 1000000

toNextPrompt :: Config -> Handle -> IO String
toNextPrompt config from = do
  readUntil (replPrompt config) from

executeReplAction :: Config -> Handle -> Handle -> Handle -> IO String
executeReplAction config to from fromStdErr = do
  (errOutput, stdOutput) <- captureWhile fromStdErr $ do
    hPutStrLn to (replAction config) >> hFlush to
    toNextPrompt config from
  hPutStrLn stderr errOutput >> hFlush stderr
  return (errOutput ++ "\n===\n" ++ stdOutput)

app :: Config -> Handle -> Handle -> Handle -> Application
app config to from fromStdErr _request respond = do
  output <- executeReplAction config to from fromStdErr
  respond $ responseLBS ok200 [] (cs output)

withApplication :: Application -> IO a -> IO a
withApplication application action = do
  removeSocketFile
  withSocket $ \sock -> do
    bracket_ (bind sock socketAddr) removeSocketFile $ do
      listen sock maxListenQueue
      withThread (runSettingsSocket defaultSettings sock application) action

withThread :: IO () -> IO a -> IO a
withThread asyncAction action = do
  mvar <- newEmptyMVar
  tid <- forkIO $ do
    asyncAction `finally` putMVar mvar ()
  r <- action
  killThread tid
  takeMVar mvar
  return r
