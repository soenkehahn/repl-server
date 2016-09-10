
module ReplClient where

import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Network.HTTP.Client
import           Network.Socket hiding (recv)
import           Prelude ()
import           Prelude.Compat

import           Socket

replClient :: IO ByteString
replClient = do
  let newConnection _ _ _ = do
        sock <- newSocket
        connect sock socketAddr
        socketConnection sock 8192
  manager <- newManager defaultManagerSettings {managerRawConnection = return newConnection}
  request <- parseUrl "http://localhost/" -- dummy url
  response <- httpLbs request {checkStatus = \_ _ _ -> Nothing} manager
  return $ responseBody response
