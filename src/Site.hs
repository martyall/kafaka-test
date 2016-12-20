{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Site
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Types


type API = "users" :> Get '[JSON] ()

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = undefined
