{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Database.Beam.Postgres
import Control.Monad.Reader
import Database.Beam
import Schema
import Utils
import Conf
import Data.Pool
import System.Environment
import Data.Text.Encoding as TSE
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU
import System.Directory.Internal.Prelude
import Data.Text.Short
import Data.Password.Argon2

data AccountData = AccountData { account :: Text
                               , password :: Text
                               } deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance FromForm AccountData

type AuthAPI = "auth" :> ("sign_up" :> (Get '[HTML] RawHtml
                                   :<|> ReqBody '[FormUrlEncoded] AccountData :> PostRedirect 302 RedirectUrl)
                     :<|> "sign_in" :> (Get '[HTML] RawHtml
                                   :<|> ReqBody '[FormUrlEncoded] AccountData :> PostRedirect 302 RedirectUrl))

authAPI :: Proxy AuthAPI
authAPI = Proxy

authServerReader :: Reader Env (Server AuthAPI)
authServerReader = asks $ \env ->
  hoistServer authAPI (readerToHandler env) authServerT

authServerT :: ServerT AuthAPI ReaderHandler
authServerT = (authSignUpGetHandler
         :<|>  authSignUpPostHandler)
         :<|> (authSignInGetHandler
         :<|>  authSignInPostHandler)

  where authSignUpGetHandler :: ReaderHandler RawHtml
        authSignUpGetHandler = Template.htmlHandler mempty "/sign_up.html"

        authSignUpPostHandler :: (ToHttpApiData loc, IsString loc) =>
          AccountData -> ReaderHandler (Headers '[Header "Location" loc] NoContent)
        authSignUpPostHandler accountData = do
          pool <- asks pool
          hashedPassword <- hashPassword $ mkPassword $ password accountData
          liftIO $ insertAccount pool hashedPassword
          redirect "/auth/sign_in"

          where insertAccount :: Pool Connection -> PasswordHash Argon2 -> IO ()
                insertAccount pool (PasswordHash hashedPassword) = withResource pool $ \conn -> runBeamPostgres conn $
                  runInsert $ Database.Beam.insert (_healthAccount healthDb) $
                  insertExpressions [Account default_ (val_ $ account accountData) (val_ hashedPassword)]

        authSignInGetHandler :: ReaderHandler RawHtml
        authSignInGetHandler = Template.htmlHandler mempty "/sign_in.html"

        authSignInPostHandler :: (ToHttpApiData loc, IsString loc) =>
          AccountData -> ReaderHandler (Headers '[Header "Location" loc] NoContent)
        authSignInPostHandler accountData = do
          pool <- asks pool
          acc <- liftIO $ selectAccount pool accountData
          password <- return $ mkPassword $ password accountData
          liftIO $ print $ "check for '" <> account accountData <> "'"
          passwordCheck <- isPasswordOk password acc
          liftIO $ print passwordCheck
          redirect "/html"

            where selectAccount pool accountData = withResource pool $ \conn -> runBeamPostgres conn $
                    runSelectReturningOne $ select $
                    filter_ (\acc -> _accountName acc ==. val_ (account accountData)) $
                    all_ $ _healthAccount healthDb

                  isPasswordOk :: Monad m => Password -> Maybe (AccountT Identity) -> m PasswordCheck
                  isPasswordOk inputPassword Nothing        = return PasswordCheckFail
                  isPasswordOk inputPassword (Just account) = return $ (checkPassword inputPassword . PasswordHash) (_accountPassword account :: Text)
