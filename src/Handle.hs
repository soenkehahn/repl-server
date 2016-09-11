{-# LANGUAGE LambdaCase #-}

module Handle where

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
