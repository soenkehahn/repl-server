{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.ByteString.Lazy
import           Data.Proxy
import           Servant.API

type Api =
  ReqBody '[OctetStream] ByteString :> Post '[OctetStream] ByteString

api :: Proxy Api
api = Proxy
