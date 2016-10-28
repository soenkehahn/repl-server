{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ReplServer where

import           Control.Concurrent
import           Control.Exception hiding (Handler)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.String
import           Data.String.Conversions
import           Data.Text.Lazy (Text)
import           GHC.IO.Handle
import           Network.Socket hiding (recv)
import           Network.Wai
import           Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import           Prelude ()
import           Prelude.Compat
import           Servant.Server
import           System.IO
import           WithCli

import           Api
import           Handle
import           Process
import           Socket

data Config
  = Config {
    replCommand :: String,
    replPrompt :: String
  }
  deriving (Generic, Show)

instance HasArguments Config

replServer :: Config -> IO ()
replServer config = do
  withProcess (replCommand config) $ \ (to, from, errFrom, _) -> do
    let handles = ProcessHandles{to, from, errFrom}
    _ <- toNextPrompt config from
    withApplication (app config handles) $ do
      forever $ threadDelay 1000000

toNextPrompt :: Config -> Handle -> IO String
toNextPrompt config from = do
  readUntil (replPrompt config) from

data ProcessHandles
  = ProcessHandles {
    to :: Handle,
    from :: Handle,
    errFrom :: Handle
  }

executeReplAction :: Config -> ProcessHandles -> Text -> IO String
executeReplAction config ProcessHandles{to, from, errFrom} action = do
  (errOutput, stdOutput) <- captureWhile errFrom $ do
    hPutStrLn to (cs action) >> hFlush to
    toNextPrompt config from
  hPutStrLn stderr errOutput >> hFlush stderr
  return (errOutput ++ "\n===\n" ++ stdOutput)

app :: Config -> ProcessHandles -> Application
app config handles = serve api $ server config handles

server :: Config -> ProcessHandles -> Server Api
server = postAction

postAction :: Config -> ProcessHandles -> Text -> Handler Text
postAction config handles action = liftIO $ do
  output <- executeReplAction config handles action
  return $ cs output

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
