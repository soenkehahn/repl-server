{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ReplServer where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Data.String
import           Data.String.Conversions
import           Network.HTTP.Types
import           Network.Socket hiding (recv)
import           Network.Wai
import           Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import           Prelude ()
import           Prelude.Compat
import           System.IO
import           WithCli

import           Process
import           Socket

data Config
  = Config {
    replCommand :: String,
    replAction :: String
  }
  deriving (Generic, Show)

instance HasArguments Config

replServer :: Config -> IO ()
replServer config = do
  withProcess (replCommand config) $ \ (to, from, _) -> do
    mvar <- newMVar ""
    withThread (connectHandles from (mvar, stdout)) $ do
      withApplication (app config to mvar) $ do
        hPutStrLn to (replAction config)
        forever $ threadDelay 1000000

app :: Config -> Handle -> MVar ByteString -> Application
app config to mvar _request respond = do
  hPutStrLn to (replAction config)
  threadDelay 300000 -- fixme!
  output <- readMVar mvar
  respond $ responseLBS ok200 [] output

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
