
module ReplClient where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Conduit
import           Data.String.Conversions
import           Network.HTTP.Client
import           Network.Socket hiding (recv)
import           Prelude ()
import           Prelude.Compat
import           Servant.Client
import           System.Exit

import           Api
import           Socket

postAction :: LT -> Manager -> BaseUrl -> ClientM LT
postAction = client api

replClient :: LT -> IO (Source ST)
replClient action = do
  let newConnection _ _ _ = do
        sock <- newSocket
        connect sock socketAddr
        socketConnection sock 8192
  manager <- newManager defaultManagerSettings {managerRawConnection = return newConnection}
  let baseUrl = BaseUrl Http "localhost" 8080 "" -- dummy BaseUrl
  try $ postAction action manager baseUrl

try :: Show e => ExceptT e IO a -> IO a
try action = do
  r <- runExceptT action
  case r of
    Right a -> return a
    Left err -> die (show err)
