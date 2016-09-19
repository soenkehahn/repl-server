
module ReplClient (replClient) where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.String.Conversions
import           Network.HTTP.Client
import           Network.Socket hiding (recv)
import           Prelude ()
import           Prelude.Compat
import           Servant.Client

import           Api
import           Socket

apiCall :: LBS -> Manager -> BaseUrl -> ClientM LBS
apiCall = client api

replClient :: LBS -> IO LBS
replClient replAction = do
  let newConnection _ _ _ = do
        sock <- newSocket
        connect sock socketAddr
        socketConnection sock 8192
  manager <- newManager defaultManagerSettings {managerRawConnection = return newConnection}
  -- request <- parseUrl "http://localhost/" -- dummy url
  result <- runExceptT (apiCall replAction manager (BaseUrl Http "localhost" 80 ""))
  case result of
    Right r -> return r
  -- response <- httpLbs request {checkStatus = \_ _ _ -> Nothing} manager
  -- return $ responseBody response
