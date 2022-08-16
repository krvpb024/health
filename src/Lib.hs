{-# LANGUAGE TypeOperators #-}

module Lib (startApp) where

import Database.PostgreSQL.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
-- import Web.ClientSession
-- import Web.Cookie
import Configuration.Dotenv
import System.Environment
import Route.Static
import Route.Auth
import Control.Monad.Reader
import Conf

type API = AuthAPI
      :<|> StaticAPI

server :: Reader Env (Server API)
server = do
  authServer <- authServerReader
  return $ authServer
      :<|> staticServer

api :: Proxy API
api = Proxy

app :: Reader Env Application
app = serve api <$> server

startApp :: IO ()
startApp = do
  loadFile defaultConfig
  postgresUrl <- getEnv "DATABASE_URL"
  pool <- dbPool postgresUrl
  -- session <- Vault.newKey
  -- key <- getDefaultKey
  -- cDt <- encryptIO key "test"
  -- print cDt
  -- print $ decrypt key cDt
  run 8080 $ runReader app $ Env pool
