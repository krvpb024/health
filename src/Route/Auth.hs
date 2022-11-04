{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Route.Auth where

import Servant
import Control.Monad.IO.Class
import Web.FormUrlEncoded
import GHC.Generics
import Data.Aeson
import Data.String (IsString)
import Database.Beam.Postgres
import Control.Monad.Trans.Reader
import Database.Beam
import Schema
import Utils.ReaderHandler
import Conf
import Data.Pool
import Data.Password.Argon2
import Web.Cookie
import Network.Wai.Session
import Database.Beam.Backend.SQL.BeamExtensions
import Data.Either.Extra
import Control.Exception
import Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding as TSE
import Data.Text.Lazy as TL
import Data.ByteString.Lazy as BL
import Data.Time
import Servant.Server.Experimental.Auth
import Network.Wai
import Utils.AuthHandler
import Utils.TemplateHandler as TP
import Data.HashMap.Strict as HS
import Text.Ginger

data AccountData = AccountData { name     :: TL.Text
                               , password :: TL.Text
                               } deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance FromForm AccountData

type AuthAPI = "auth" :> ("sign_up" :> ( Get '[HTML] RawHtml
                                    :<|> ReqBody '[FormUrlEncoded] AccountData :> PostRedirect 303 RedirectUrl
                                       )
                     :<|> "sign_in" :> ( Get '[HTML] RawHtml
                                    :<|> ReqBody '[FormUrlEncoded] AccountData
                                           :> UVerb 'POST '[HTML] [ WithStatus 401 RawHtml
                                                                  , WithStatus 303 ( Headers '[ Header "Location" RedirectUrl
                                                                                              , Header "Set-Cookie" SetCookie
                                                                                              ] NoContent ) ]
                                       )
                     :<|> "sign_out" :> AuthProtect "cookie-auth"
                                          :> Verb 'POST 303 '[HTML] ( Headers '[ Header "Location" RedirectUrl
                                                                               , Header "Set-Cookie" SetCookie
                                                                               ] NoContent )
                         )

authAPI :: Proxy AuthAPI
authAPI = Proxy

authServerReader :: Reader Env (Server AuthAPI)
authServerReader = asks $ \env ->
  hoistServerWithContext authAPI
                         (Proxy :: Proxy '[AuthHandler Request (Maybe SignInAccount)])
                         (readerToHandler env)
                         authServerT

authServerT :: ServerT AuthAPI ReaderHandler
authServerT = (authSignUpGetHandler
         :<|>  authSignUpPostHandler)
         :<|> (authSignInGetHandler
         :<|>  authSignInPostHandler)
         :<|>  authSignOutPostHandler

  where authSignUpGetHandler :: ReaderHandler RawHtml
        authSignUpGetHandler = TP.htmlHandler mempty "/sign_up.html"

        authSignUpPostHandler :: (ToHttpApiData RedirectUrl, IsString RedirectUrl) =>
          AccountData -> ReaderHandler (Headers '[Header "Location" RedirectUrl] NoContent)
        authSignUpPostHandler accountData = do
          pool <- asks getPool
          hashedPassword <- hashPassword $ mkPassword $ TL.toStrict $ password accountData
          liftIO $ insertAccount pool hashedPassword
          redirect "/auth/sign_in"

          where insertAccount :: Pool Connection -> PasswordHash Argon2 -> IO ()
                insertAccount pool (PasswordHash hashedPassword) = withResource pool $ \conn -> runBeamPostgres conn $
                  runInsert $ Database.Beam.insert (_healthAccount healthDb) $
                  insertExpressions [ Account default_
                                              (val_ $ name accountData)
                                              (val_ $ TL.fromStrict hashedPassword)
                                    ]

        authSignInGetHandler :: ReaderHandler RawHtml
        authSignInGetHandler = TP.htmlHandler mempty "/sign_in.html"

        authSignInPostHandler :: (ToHttpApiData RedirectUrl, IsString RedirectUrl) =>
          AccountData -> ReaderHandler ( Union '[ WithStatus 401 RawHtml
                                                , WithStatus 303 ( Headers '[ Header "Location" RedirectUrl
                                                                            , Header "Set-Cookie" SetCookie
                                                                            ] NoContent ) ] )
        authSignInPostHandler accountData = do
          pool <- asks getPool
          currentTimestamp <- liftIO getCurrentTime
          account <- liftIO $ selectAccount pool accountData
          let authorizedAccount :: Either ServerError (AccountT Identity)
              authorizedAccount = isPasswordValid hashedInputPassword account
              hashedInputPassword :: Password
              hashedInputPassword = mkPassword $ TL.toStrict $ password accountData
          newSid <- liftIO $ genNewSid pool authorizedAccount
          case newSid of
            Left  err -> do html <- TP.htmlHandler context "/sign_in.html"
                            respond $ WithStatus @401 $ html
                              where context :: HashMap VarName Value
                                    context = HS.fromList [ ("globalMsgs", toJSON [TLE.decodeUtf8 (errBody err)]) ]
            Right sid -> do
              liftIO $ deleteExpiredSession pool account currentTimestamp
              timeZone <- liftIO getCurrentTimeZone
              let redirect ::Headers [Header "Location" RedirectUrl, Header "Set-Cookie" SetCookie] NoContent
                  redirect = addHeader "/profile" $
                             addHeader defaultSetCookie { setCookieName     = "servant-auth-cookie"
                                                        , setCookieValue    = BL.toStrict $ TLE.encodeUtf8 $
                                                                              _sessionId sid
                                                        , setCookieHttpOnly = True
                                                        , setCookieSecure   = True
                                                        , setCookieSameSite = Just sameSiteStrict
                                                        , setCookiePath     = Just "/"
                                                        , setCookieExpires  = Just $ _sessionExpireAt sid
                                                        }
                              NoContent
              respond $ WithStatus @303 $ redirect

            where selectAccount :: Pool Connection
                                -> AccountData
                                -> IO (Either ServerError (AccountT Identity))
                  selectAccount pool accountData = do
                    account <- withResource pool $ \conn -> runBeamPostgres conn $
                      runSelectReturningOne $ select $
                      filter_ (\account -> _accountName account ==. val_ (name accountData)) $
                      all_ $ _healthAccount healthDb
                    return $ maybeToEither (err401 { errBody = "No Such User." }) account

                  isPasswordValid :: Password
                                  -> Either ServerError (AccountT Identity)
                                  -> Either ServerError (AccountT Identity)
                  isPasswordValid _             (Left  err) = Left err
                  isPasswordValid inputPassword (Right acc) =
                    case (checkPassword inputPassword . PasswordHash) $ TL.toStrict (_accountPassword acc :: TL.Text) of
                      PasswordCheckFail    -> Left err401 { errBody = "Password Incorrect." }
                      PasswordCheckSuccess -> return acc


                  genNewSid :: Pool Connection
                            -> Either ServerError (AccountT Identity)
                            -> IO (Either ServerError (SessionT Identity))
                  genNewSid _    (Left e)        = return $ Left e
                  genNewSid pool (Right account) = do
                    sid <- genSessionId
                    [newSession] <- withResource pool $ \conn -> runBeamPostgres conn $
                      runInsertReturningList $ Database.Beam.insert (_healthSession healthDb) $
                      insertExpressions [Session (val_ $ TL.fromStrict $ TSE.decodeUtf8 sid) (val_ $ primaryKey account) default_]
                    return $ return newSession

                  deleteExpiredSession :: Pool Connection
                                       -> Either ServerError (AccountT Identity)
                                       -> UTCTime
                                       -> IO ()
                  deleteExpiredSession _    (Left err)      _                = throw err
                  deleteExpiredSession pool (Right account) currentTimestamp = do
                    withResource pool $ \conn -> runBeamPostgres conn $
                      runDelete $ Database.Beam.delete (_healthSession healthDb) $
                      \session -> (_sessionAccountId session ==. val_ (primaryKey account)) &&.
                                  (_sessionExpireAt session <=. val_ currentTimestamp)

        authSignOutPostHandler:: (ToHttpApiData RedirectUrl, IsString RedirectUrl) =>
             Maybe SignInAccount -> ReaderHandler (Headers '[ Header "Location" RedirectUrl
                                                            , Header "Set-Cookie" SetCookie
                                                            ] NoContent)
        authSignOutPostHandler Nothing        = return $ addHeader "/" $
                                                         noHeader
                                                        NoContent
        authSignOutPostHandler (Just account) = do
          pool <- asks getPool
          liftIO $ withResource pool $ \conn -> runBeamPostgres conn $
            runDelete $ Database.Beam.delete (_healthSession healthDb) $
            \session -> _sessionId session ==. val_ (sessionId account)
          return $ addHeader "/" $
                   addHeader defaultSetCookie { setCookieName     = "servant-auth-cookie"
                                              , setCookieValue    = mempty
                                              , setCookieHttpOnly = True
                                              , setCookieSecure   = True
                                              , setCookieSameSite = Just sameSiteStrict
                                              , setCookiePath     = Just "/"
                                              , setCookieMaxAge   = Just $ secondsToDiffTime (-1)
                                              }
                   NoContent
