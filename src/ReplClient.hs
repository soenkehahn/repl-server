
module ReplClient where

import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Network.HTTP.Client
import           Network.HTTP.Types
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
  request <- do
    r <- parseUrlThrow "http://localhost/" -- dummy url
    return $ r{
      method = methodPost
    }
  response <- httpLbs request {checkStatus = \_ _ _ -> Nothing} manager
  return $ responseBody response
