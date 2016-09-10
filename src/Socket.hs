
module Socket where

import           Control.Exception
import           Control.Monad
import           Data.String
import           Network.HTTP.Client.Internal (Connection, makeConnection)
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (sendAll, recv)
import           Prelude ()
import           Prelude.Compat
import           System.Directory
import           System.IO.Error

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

socketConnection :: Socket -> Int -> IO Connection
socketConnection sock chunksize = makeConnection (recv sock chunksize) (sendAll sock) (sClose sock)
