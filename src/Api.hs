{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Text.Lazy
import           Data.Proxy
import           Servant.API

type Api =
  ReqBody '[JSON] (Maybe Text) :> Post '[PlainText] Text

api :: Proxy Api
api = Proxy
