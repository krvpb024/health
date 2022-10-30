{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (startApp) where

import Database.PostgreSQL.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Configuration.Dotenv
import System.Environment
import Route.Static
import Route.Auth
import Control.Monad.Trans.Reader
import Conf
import Utils.TemplateHandler
import Data.HashMap.Strict as HS
import Utils.AuthHandler
import Route.Profile

type BaseAPI = Get '[HTML] RawHtml

baseAPI :: Proxy BaseAPI
baseAPI = Proxy

baseServer :: Server BaseAPI
baseServer = baseHandler
  where baseHandler = Utils.TemplateHandler.htmlHandler mempty "/index.html"

type API = AuthAPI
      :<|> ProfileAPI
      :<|> StaticAPI
      :<|> BaseAPI

server :: Reader Env (Server API)
server = do
  authServer <- authServerReader
  profileServer <- profileServerReader
  return $ authServer
      :<|> profileServer
      :<|> staticServer
      :<|> baseServer

api :: Proxy API
api = Proxy

app :: Env -> Application
app env = serveWithContext api (genAuthServerContext env) $ runReader server env

startApp :: IO ()
startApp = do
  loadFile defaultConfig
  postgresUrl <- getEnv "DATABASE_URL"
  pool <- dbPool postgresUrl
  run 8080 $ app $ Env pool
