{-# LANGUAGE LambdaCase #-}

module Handle where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Function
import           Data.Monoid
import           System.IO

readUntil :: String -> Handle -> IO String
readUntil needle h = inner "" "" needle
  where
    inner acc matched = \ case
      (a : r) -> do
        c <- hGetChar h
        hPutChar stdout c >> hFlush stdout
        if c == a
          then inner acc (c : matched) r
          else inner (c : matched ++ acc) "" needle
      [] -> return $ reverse acc

captureWhile :: Handle -> IO a -> IO (String, a)
captureWhile handle action = do
  outputMVar <- newMVar ""
  killedMVar <- newEmptyMVar
  tid <- forkIO $ forever (relayChar outputMVar)
    `catch` readRest outputMVar killedMVar
  result <- action
  throwTo tid ReadRest
  readMVar killedMVar
  output <- readMVar outputMVar
  return (output, result)

  where
    relayChar :: MVar String -> IO ()
    relayChar mvar = do
      eof <- hIsEOF handle
      when (not eof) $ do
        c <- hGetChar handle
        modifyMVar_ mvar $ \ old -> do
          return (old <> [c])

    readRest :: MVar String -> MVar () -> ReadRest -> IO ()
    readRest outputMVar killedMVar ReadRest = do
      fix $ \ loop -> do
        ready <- hReady handle
        when ready $ do
          relayChar outputMVar
          loop
      putMVar killedMVar ()

data ReadRest = ReadRest
  deriving (Show)

instance Exception ReadRest
