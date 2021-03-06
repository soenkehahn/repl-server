
module ProcessSpec where

import           Control.Concurrent
import           Control.Monad
import           Data.List
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process
import           Test.Hspec
import           Test.Mockery.Directory

import           Process

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "withProcess" $ do
    it "runs the given process concurrently" $ do
      withProcess "touch file" $ \ _ -> do
        waitForFile "file"
      readFile "file" `shouldReturn` ""

    it "kills the process when the invoking thread is killed" $ do
      mvar <- newEmptyMVar
      withProcess "ghci" $ \ (_, _, _, process) -> do
        putMVar mvar process
      process <- takeMVar mvar
      getProcessExitCode process `shouldReturn` Just (ExitFailure (- 15))

    it "allows to interact with stdin and stdout of the process" $ do
      withProcess "ghci" $ \ (to, from, _, _) -> do
        hPutStrLn to "map succ \"aaa\""
        _ <- hGetLine from -- skipping one line of ghci info
        result <- hGetLine from
        result `shouldSatisfy` ("bbb" `isInfixOf`)
        hPutStrLn to ":quit"

waitForFile :: FilePath -> IO ()
waitForFile file = do
  files <- getDirectoryContents "."
  when (not (file `elem` files)) $
    waitForFile file
