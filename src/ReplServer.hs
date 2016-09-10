{-# LANGUAGE DeriveGeneric #-}

module ReplServer where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.String
import           Network.HTTP.Types
import           Network.Socket
import           Network.Wai
import           Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import           Prelude ()
import           Prelude.Compat
import           System.Directory
import           System.IO
import           System.IO.Error
import           System.Posix.Files
import           System.Posix.Signals
import           System.Process
import           WithCli

import           Process

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
    withThread (connectHandles from stdout) $ do
      hPutStrLn to (replAction config)
      forever $ threadDelay 1000000

connectHandles :: Handle -> Handle -> IO ()
connectHandles from to = inner
  where
    inner = do
      eof <- hIsEOF from
      when (not eof) $ do
        c <- hGetChar from
        hPutChar to c
        inner

withApplication :: Application -> IO a -> IO a
withApplication application action = do
  removeSocketFile
  withSocket $ \sock -> do
    bracket_ (bind sock socketAddr) removeSocketFile $ do
      listen sock maxListenQueue
      withThread (runSettingsSocket defaultSettings sock application) action

socketName :: String
socketName = ".repl-server.socket"

socketAddr :: SockAddr
socketAddr = SockAddrUnix socketName

newSocket :: IO Socket
newSocket = socket AF_UNIX Stream 0

withSocket :: (Socket -> IO a) -> IO a
withSocket action = bracket newSocket close action

removeSocketFile :: IO ()
removeSocketFile = void $ tryJust (guard . isDoesNotExistError) (removeFile socketName)

withThread :: IO () -> IO a -> IO a
withThread asyncAction action = do
  mvar <- newEmptyMVar
  tid <- forkIO $ do
    asyncAction `finally` putMVar mvar ()
  r <- action
  killThread tid
  takeMVar mvar
  return r
