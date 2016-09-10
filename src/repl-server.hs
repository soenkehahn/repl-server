
import           WithCli

import           ReplServer
import           System.IO


main :: IO ()
main = withCli $ \ config -> do
  replServer config stdout
