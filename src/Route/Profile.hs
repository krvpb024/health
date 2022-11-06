
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Route.Profile where

import Servant
import Utils.TemplateHandler as TP
import Utils.ReaderHandler
import Control.Monad.Trans.Reader
import Conf
import Utils.AuthHandler
import Servant.Server.Experimental.Auth
import Network.Wai
import qualified Data.HashMap.Strict as HS
import Data.Int
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Data.Aeson
import Data.Time
import Control.Monad.IO.Class
import Web.FormUrlEncoded
import Data.Pool
import Database.Beam.Postgres
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Schema
import Data.Bool
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Either.Combinators
import Control.Exception
import qualified Data.Text as T

type ProfileAPI =  "profile" :> ( AuthProtect "cookie-auth"
                                    :> UVerb 'GET '[HTML] [ WithStatus 403 RawHtml
                                                          , WithStatus 404 RawHtml
                                                          , WithStatus 200 RawHtml
                                                          ]
                             :<|> AuthProtect "cookie-auth"
                                    :> "create"
                                    :> UVerb 'GET '[HTML] [ WithStatus 403 RawHtml
                                                          , WithStatus 200 RawHtml
                                                          ]
                             :<|> AuthProtect "cookie-auth"
                                    :> ReqBody '[FormUrlEncoded] ProfilePostData
                                    :> UVerb 'POST '[HTML] [ WithStatus 403 RawHtml
                                                           , WithStatus 303 ( Headers '[ Header "Location" RedirectUrl
                                                                                       ] NoContent )
                                                           ]
                             :<|> AuthProtect "cookie-auth"
                                    :> "edit"
                                    :> UVerb 'GET '[HTML] [ WithStatus 403 RawHtml
                                                          , WithStatus 404 RawHtml
                                                          , WithStatus 200 RawHtml
                                                          ]
                             :<|> AuthProtect "cookie-auth"
                                    :> "put"
                                    :> ReqBody '[FormUrlEncoded] ProfilePostData
                                    :> UVerb 'POST '[HTML] [ WithStatus 403 RawHtml
                                                           , WithStatus 404 RawHtml
                                                           , WithStatus 303 ( Headers '[ Header "Location" RedirectUrl
                                                                                       ] NoContent )
                                                           ]
                                )

data ProfilePostData = ProfilePostData {
    birthDate :: Day
  , gender    :: Bool
  , height    :: Double
} deriving (Eq, Show, Generic)

instance FromJSON ProfilePostData where
  parseJSON = withObject "ProfilePostData" $ \v -> ProfilePostData
      <$> v .: "birthDate"
      <*> (bool False True . (("male" :: TL.Text) ==) <$> (v .: "gender"))
      <*> v .: "height"

instance ToJSON ProfilePostData where
  toJSON (ProfilePostData birthDate gender height) =
      object [ "birthDate" .= birthDate
             , "gender"    .= bool ("女" :: TL.Text) "男" gender
             , "height"    .= height
             ]

instance FromForm ProfilePostData where
  fromForm f = ProfilePostData
    <$> parseUnique "birthDate" f
    <*> (bool False True . (("male" :: TL.Text) ==) <$> parseUnique "gender" f)
    <*> parseUnique "height" f

profileAPI :: Proxy ProfileAPI
profileAPI = Proxy

profileServerReader :: Reader Env (Server ProfileAPI)
profileServerReader = asks $ \env ->
  hoistServerWithContext profileAPI
                         (Proxy :: Proxy '[AuthHandler Request (Maybe SignInAccount)])
                         (readerToHandler env)
                         profileServerT

profileServerT :: ServerT ProfileAPI ReaderHandler
profileServerT = profileGetHandler
            :<|> profileGetCreateFormHandler
            :<|> profilePostHandler
            :<|> profileGetEditFormHandler
            :<|> profilePutHandler

  where profileGetHandler :: Maybe SignInAccount
                          -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                    , WithStatus 404 RawHtml
                                                    , WithStatus 200 RawHtml ] )
        profileGetHandler Nothing = respond =<< liftIO authFailToSignInView
        profileGetHandler (Just account) = do
          pool <- asks getPool
          maybeProfile <- liftIO $ selectProfile pool account
          case maybeProfile of
            Left err -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @404 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 $ errBody err] )]
            Right profile -> do
              html <- TP.htmlHandler context "/profile.html"
              respond $ WithStatus @200 html
              where context = HS.fromList [ ( "accountName", toJSON $ accountName account )
                                          , ( "profileId", toJSON $ _profileId profile )
                                          , ( "profileGender", toJSON $ bool ("女" :: TL.Text) "男" $ _profileGender profile )
                                          , ( "profileBirthDate", toJSON $ _profileBirthDate profile )
                                          ]

        profileGetCreateFormHandler :: Maybe SignInAccount
                                    -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                              , WithStatus 200 RawHtml ] )
        profileGetCreateFormHandler Nothing        = respond =<< liftIO authFailToSignInView
        profileGetCreateFormHandler (Just account) = do
          html <- TP.htmlHandler mempty "/profile_form.html"
          respond $ WithStatus @200 html

        profilePostHandler :: Maybe SignInAccount
                           -> ProfilePostData
                           -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                     , WithStatus 303 (Headers '[ Header "Location" RedirectUrl] NoContent )
                                                     ] )
        profilePostHandler Nothing _ = respond =<< liftIO authFailToSignInView
        profilePostHandler (Just account) profileFormData = do
          pool <- asks getPool
          profile <- liftIO $ insertProfile pool account profileFormData
          red <- redirect ("/auth/sign_in" :: RedirectUrl)
          respond $ WithStatus @303 red
          where
                insertProfile :: Pool Connection -> SignInAccount -> ProfilePostData -> IO Profile
                insertProfile pool account profileFormData = do
                  [profile] <- withResource pool $ \conn -> runBeamPostgres conn $
                    runInsertReturningList $ Database.Beam.insert (_healthProfile healthDb) $
                    insertExpressions [ Profile { _profileId         = default_
                                                , _profileAccountId  = AccountId $ val_ $ accountId account
                                                , _profileGender     = val_ $ gender profileFormData
                                                , _profileBirthDate  = val_ $ birthDate profileFormData
                                                , _profileHeight = val_ $ height profileFormData
                                                } ]
                  pure profile

        profileGetEditFormHandler :: Maybe SignInAccount
                                  -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                            , WithStatus 404 RawHtml
                                                            , WithStatus 200 RawHtml ] )
        profileGetEditFormHandler Nothing = respond =<< liftIO authFailToSignInView
        profileGetEditFormHandler (Just account) = do
          pool <- asks getPool
          profile <- liftIO $ selectProfile pool account
          let authorizedProfile = profile >>= checkHasProfilePermission account
          case authorizedProfile of
            Left (ServerError 404 _ errBody _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @404 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 errBody] )]
            Left (ServerError 403 _ errBody _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @403 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 errBody] )]
            Right profile -> do
              html <- TP.htmlHandler context "/profile_form_edit.html"
              respond $ WithStatus @200 html
              where context = HS.fromList [ ( "profileId", toJSON $ _profileId profile )
                                          , ( "profileData", toJSON $ ProfilePostData (_profileBirthDate profile)
                                                                                      (_profileGender profile)
                                                                                      (_profileHeight profile) )
                                          ]
            _ -> throw err500

        profilePutHandler :: Maybe SignInAccount
                          -> ProfilePostData
                          -> ReaderHandler( Union '[ WithStatus 403 RawHtml
                                                   , WithStatus 404 RawHtml
                                                   , WithStatus 303 ( Headers '[ Header "Location" RedirectUrl
                                                                               ] NoContent ) ] )
        profilePutHandler Nothing _ = respond =<< liftIO authFailToSignInView
        profilePutHandler (Just account) profilePutData = do
          pool <- asks getPool
          profile <- liftIO $ selectProfile pool account
          let authorizedProfile = profile >>= checkHasProfilePermission account
          case authorizedProfile of
            Left (ServerError 404 _ errBody _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @404 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 errBody] )]
            Left (ServerError 403 _ errBody _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @403 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 errBody] )]
            Right profile -> do
              liftIO $ updateProfile pool profile profilePutData
              red <- redirect ("/profile" :: RedirectUrl)
              respond $ WithStatus @303 red
              where
                    updateProfile pool profile profilePutData =
                      withResource pool $ \conn -> runBeamPostgres conn $ do
                        runUpdate $ save (_healthProfile healthDb) ( profile {
                            _profileGender = gender profilePutData
                          , _profileBirthDate = birthDate profilePutData
                          , _profileHeight = height profilePutData
                        } )
            _ -> throw err500

selectProfile :: Pool Connection -> SignInAccount -> IO (Either ServerError Profile)
selectProfile pool signInAccount = do
  profile <- withResource pool $ \conn -> runBeamPostgres conn $
    runSelectReturningOne $ select $
      filter_ (((AccountId . val_ . accountId) signInAccount ==.) . _profileAccountId) $
      all_ $ _healthProfile healthDb
  pure $ maybeToRight err404 { errBody = TLE.encodeUtf8 "檔案不存在"} profile

checkHasProfilePermission :: SignInAccount -> Profile -> Either ServerError Profile
checkHasProfilePermission signInAccount profile
  | _profileAccountId profile == (AccountId . accountId) signInAccount = Right profile
  | otherwise = Left err403 { errBody = TLE.encodeUtf8 "你沒有權限進行編輯" }
