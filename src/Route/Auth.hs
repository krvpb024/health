{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Route.Auth where

import Servant
import Template
import Data.HashMap.Strict as HS
import Data.Text
import GHC.TypeLits
import Control.Monad.IO.Class
import Web.FormUrlEncoded
import GHC.Generics
import Data.Aeson
import Data.String (IsString)
-- import Crypto.Argon2
import Database.Beam.Postgres
import Control.Monad.Reader
import Database.Beam
import Schema
import Utils
import Conf
import Data.Pool

data SignUpData = SignUpData { account :: Text
                             , password :: Text
                             } deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance FromForm SignUpData

type AuthAPI = "auth" :> "sign_up" :> (Get '[HTML] RawHtml
                                  :<|> ReqBody '[FormUrlEncoded] SignUpData :> PostRedirect 302 String
                                      )
authAPI :: Proxy AuthAPI
authAPI = Proxy

authServerReader :: Reader Env (Server AuthAPI)
authServerReader = do
  env <- ask
  return $ hoistServer authAPI (readerToHandler env) authServerT
  where readerToHandler :: Env -> ReaderHandler a -> Handler a
        readerToHandler env reader = runReaderT reader env

authServerT :: ServerT AuthAPI ReaderHandler
authServerT = authGetHandler
         :<|> authPostHandler

  where authGetHandler :: ReaderHandler RawHtml
        authGetHandler = Template.htmlHandler mempty "/sign_up.html"

        authPostHandler :: (ToHttpApiData loc, IsString loc) => SignUpData
                                                             -> ReaderHandler (Headers '[Header "Location" loc] NoContent)
        authPostHandler signUpData = do
          pool <- asks pool
          liftIO $ withResource pool $ \conn -> runBeamPostgres conn $
              runInsert $ Database.Beam.insert (_healthAccount healthDb) $
              insertExpressions [Account default_ (val_ $ account signUpData) (val_ $ password signUpData)]
          redirect "/html"
