{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Site
    ( startApp
    ) where

import Control.Monad.Reader (ReaderT(..))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO            (FilePath)

import QueryAlgebra
import Types hiding (Handler)


type API =
       "users" :> ReqBody '[JSON] NewUser :> Post '[JSON] User
  :<|> "users" :> Capture "userId" UserId :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Put '[JSON] User
  :<|> "users" :> Capture "userId" UserId :> Delete '[JSON] ()
--
--  :<|>  "media" :> Capture "mediaId" MediaId :> Get '[JSON] Media
--  :<|>  "media" :> ReqBody '[JSON] NewMedia :> Post '[JSON] Media
--  :<|>  "media" :> ReqBody '[JSON] Media :> Put '[JSON] Media
--  :<|>  "media" :> Capture "mediaId" MediaId :> Delete '[JSON] ()

type AppHandler = ReaderT AppEnv Handler

createUser :: NewUser -> AppHandler User
createUser nUsr = interpretCrud $ createOp SCrudUser nUsr

readUser :: UserId -> AppHandler User
readUser uId = interpretCrud $ readOp SCrudUser uId

updateUser :: User -> AppHandler User
updateUser usr = interpretCrud $ updateOp SCrudUser usr

deleteUser :: UserId -> AppHandler ()
deleteUser uId = interpretCrud $ deleteOp SCrudUser uId

startApp :: FilePath -> IO ()
startApp fp = do
  e <- mkAppEnv fp
  run 8080 $ app e

app :: AppEnv -> Application
app e = serve api $ enter (phi e) server
  where
    phi :: AppEnv -> ReaderT AppEnv Handler :~> Handler
    phi e = Nat $ \m -> runReaderT m e

api :: Proxy API
api = Proxy

server :: ServerT API (ReaderT AppEnv Handler)
server = createUser
    :<|> readUser
    :<|> updateUser
    :<|> deleteUser
