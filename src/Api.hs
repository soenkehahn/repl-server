{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.ByteString
import           Data.Proxy
import           Servant.API

type Api =
  Post '[OctetStream] ByteString

api :: Proxy Api
api = Proxy
