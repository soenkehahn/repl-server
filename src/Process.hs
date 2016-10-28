
module Process where

import           Control.Exception
import           System.IO
import           System.Process

-- | Runs an external process while the given action is being executed.
withProcess :: String -> ((Handle, Handle, Handle, ProcessHandle) -> IO a) -> IO a
withProcess command action = do
  -- installHandler sigINT (CatchOnce (terminateProcess processHandle)) Nothing
  let process = (shell command){
        std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = CreatePipe
      }
  bracket (createProcess process) killProcess $
    \ (Just pStdIn, Just pStdOut, Just pStdErr, processHandle) -> do
      hSetBuffering pStdIn NoBuffering
      hSetBuffering pStdOut NoBuffering
      action (pStdIn, pStdOut, pStdErr, processHandle)
  where
    killProcess (_, _, _, process) = do
      terminateProcess process
      waitForProcess process

{-
main :: IO ()
main = do
  withProcess "stack exec -- ghci" $ \ (input, output, errOutput, ph) -> do
    hSetBuffering stdout NoBuffering
    Log.info "started"
    forkIO $ do
      hGetContents output >>= putStrLn
    forkIO $ do
      hGetContents errOutput >>= putStrLn
    threadDelay 1000000
    Log.info "sending command..."
    hPutStrLn input "import Control.Concurrent"
    hPutStrLn input "import Control.Monad"
    hPutStrLn input "forkOS $ forever $ print ()"
    hFlush input
    forever $ threadDelay 1000000 -}
