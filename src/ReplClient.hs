
module ReplClient where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.ByteString.Lazy (ByteString)
import           Network.HTTP.Client
import           Network.Socket hiding (recv)
import           Prelude ()
import           Prelude.Compat
import           Servant.Client
import           System.Exit

import           Api
import           Socket

postAction :: Manager -> BaseUrl -> ClientM ByteString
postAction = client api

replClient :: IO ByteString
replClient = do
  let newConnection _ _ _ = do
        sock <- newSocket
        connect sock socketAddr
        socketConnection sock 8192
  manager <- newManager defaultManagerSettings {managerRawConnection = return newConnection}
  let baseUrl = BaseUrl Http "localhost" 8080 "" -- dummy BaseUrl
  try $ postAction manager baseUrl

try :: Show e => ExceptT e IO a -> IO a
try action = do
  r <- runExceptT action
  case r of
    Right a -> return a
    Left err -> die (show err)
