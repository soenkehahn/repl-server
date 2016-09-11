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
  withProcess (replCommand config) $ \ (to, from, _) -> do
    toNextPrompt config from
    executeReplAction config to from
    withApplication (app config to from) $ do
      forever $ threadDelay 1000000

toNextPrompt :: Config -> Handle -> IO String
toNextPrompt config from = do
  output <- readUntil (replPrompt config) from
  return output

executeReplAction :: Config -> Handle -> Handle -> IO String
executeReplAction config to from = do
  hPutStrLn to (replAction config) >> hFlush to
  toNextPrompt config from

app :: Config -> Handle -> Handle -> Application
app config to from _request respond = do
  output <- executeReplAction config to from
  respond $ responseLBS ok200 [] (cs output)

connectHandles :: Handle -> (MVar ByteString, Handle) -> IO ()
connectHandles from (mvar, to) = inner
  where
    inner = do
      eof <- hIsEOF from
      when (not eof) $ do
        c <- hGetChar from
        hPutChar to c
        modifyMVar_ mvar (\ old -> return (old <> cs [c]))
        inner

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
