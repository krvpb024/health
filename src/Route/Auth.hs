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
                     :<|> "change_password" :> "form"
                        :> AuthProtect "cookie-auth"
                        :> UVerb 'GET '[HTML] [ WithStatus 403 RawHtml
                                              , WithStatus 404 RawHtml
                                              , WithStatus 200 RawHtml
                                              ]
                     :<|> "change_password"
                        :> AuthProtect "cookie-auth"
                        :> ReqBody '[FormUrlEncoded] ChangePasswordData
                        :> UVerb 'POST '[HTML] [ WithStatus 403 RawHtml
                                               , WithStatus 401 RawHtml
                                               , WithStatus 303 (Headers '[ Header "Location" RedirectUrl] NoContent )
                                               ]
                         )

data AccountData = AccountData { name     :: TL.Text
                               , password :: TL.Text
                               } deriving (Eq, Show, Generic, ToJSON, FromJSON, FromForm)

data ChangePasswordData = ChangePasswordData {
    originPassword  :: TL.Text
  , newPassword     :: TL.Text
  , confirmPassword :: TL.Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON, FromForm)

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
         :<|>  authChangePasswordFormHandler
         :<|>  authChangePasswordPostHandler

  where authSignUpGetHandler :: ReaderHandler RawHtml
        authSignUpGetHandler = TP.htmlHandler mempty "/sign_up.html"

        authSignUpPostHandler :: AccountData
                              -> ReaderHandler (Headers '[Header "Location" RedirectUrl] NoContent)
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

        authSignInPostHandler :: AccountData
                              -> ReaderHandler ( Union '[ WithStatus 401 RawHtml
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
                            respond $ WithStatus @401 html
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
              respond $ WithStatus @303 redirect

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

        authSignOutPostHandler:: Maybe SignInAccount
                              -> ReaderHandler (Headers '[ Header "Location" RedirectUrl
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

        authChangePasswordFormHandler :: Maybe SignInAccount
                                      -> ReaderHandler( Union '[ WithStatus 403 RawHtml
                                                               , WithStatus 404 RawHtml
                                                               , WithStatus 200 RawHtml
                                                               ] )
        authChangePasswordFormHandler Nothing = respond =<< liftIO authFailToSignInView
        authChangePasswordFormHandler (Just account) = do
          pool <- asks getPool
          account <- liftIO $ selectAccount pool account
          case account of
            Left err -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @404 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 $ errBody err] )]
            Right account -> do
              html <- TP.htmlHandler mempty "/account_form_edit.html"
              respond $ WithStatus @200 html

        authChangePasswordPostHandler :: Maybe SignInAccount
                                      -> ChangePasswordData
                                      -> ReaderHandler( Union '[ WithStatus 403 RawHtml
                                                               , WithStatus 401 RawHtml
                                                               , WithStatus 303 ( Headers '[ Header "Location" RedirectUrl] NoContent
                                                                                ) ] )
        authChangePasswordPostHandler Nothing _ = respond =<< liftIO authFailToSignInView
        authChangePasswordPostHandler (Just account) passwordData = do
          pool <- asks getPool
          account <- liftIO $ selectAccount pool account
          originPasswordCorrectAccount <- liftIO $
                                          checkOriginPasswordCorrect (originPassword passwordData)
                                                                     account
          passwordConfirmCorrectAccount <- liftIO $
                                           checkPasswordConfirmCorrect (newPassword passwordData)
                                                                       (confirmPassword passwordData)
                                                                       originPasswordCorrectAccount
          case passwordConfirmCorrectAccount of
            Left (ServerError 401 _ errBody _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @401 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 errBody] )]
            Right (account, newPassword) -> do
              hashedPassword <- liftIO $ hashPassword $ mkPassword $ TL.toStrict newPassword
              liftIO $ updatePassword pool account hashedPassword
              red <- redirect ("/profile" :: RedirectUrl)
              respond $ WithStatus @303 red
            _ -> throwError err500
          where
                checkOriginPasswordCorrect :: TL.Text
                                           -> Either ServerError Account
                                           -> IO (Either ServerError Account)
                checkOriginPasswordCorrect password account =
                  case account of
                    Left err -> return $ Left err
                    Right account -> do
                      let originPassword = PasswordHash { unPasswordHash = TL.toStrict $ _accountPassword account}
                          inputPassword = mkPassword $ TL.toStrict password
                      case checkPassword inputPassword originPassword of
                          PasswordCheckFail    -> return $ Left err401 { errBody = "Password incorrect." }
                          PasswordCheckSuccess -> return $ return account
                checkPasswordConfirmCorrect :: TL.Text
                                            -> TL.Text
                                            -> Either ServerError Account
                                            -> IO (Either ServerError (Account, TL.Text))
                checkPasswordConfirmCorrect newPassword confirmPassword account =
                  case account of
                    Left err -> return $ Left err
                    Right account -> do
                      let newPass = mkPassword $ TL.toStrict newPassword
                      hashedConfirmPassword <- hashPassword $ mkPassword $ TL.toStrict confirmPassword
                      case checkPassword newPass hashedConfirmPassword of
                        PasswordCheckFail    -> return $ Left err401 { errBody = "Password confirm incorrect." }
                        PasswordCheckSuccess -> return $ return (account, confirmPassword)
                updatePassword :: Pool Connection
                               -> Account
                               -> PasswordHash Argon2
                               -> IO ()
                updatePassword pool account (PasswordHash hashedPassword) = do
                  withResource pool $ \conn -> runBeamPostgres conn $ do
                    runUpdate $ save (_healthAccount healthDb) ( account {
                      _accountPassword = TL.fromStrict hashedPassword
                    } )



selectAccount :: Pool Connection -> SignInAccount -> IO (Either ServerError Account)
selectAccount pool signInAccount = do
  account <- withResource pool $ \conn -> runBeamPostgres conn $
    runSelectReturningOne $ select $
    filter_ ((val_ (accountId signInAccount) ==.) . _accountId) $
    all_ $ _healthAccount healthDb
  return $ maybeToEither (err404 { errBody = "No Such User." }) account
