{-# LANGUAGE DeriveGeneric #-}

module ReplServer where

import           Control.Concurrent
import           Control.Monad
import           System.IO
import           System.Posix.Signals
import           System.Process
import           WithCli

data Config
  = Config {
    replCommand :: String,
    replAction :: String
  }
  deriving (Generic, Show)

instance HasArguments Config

run :: Config -> Handle -> IO ()
run config outHandle = do
  let process = (shell (replCommand config)){
        std_in = CreatePipe,
        std_out = UseHandle outHandle
      }
  (Just pStdIn, Nothing, Nothing, processHandle) <- createProcess process
  installHandler sigINT (CatchOnce (terminateProcess processHandle)) Nothing
  hPutStrLn pStdIn $ replAction config
  hFlush pStdIn
  waitForProcess processHandle
  return ()
