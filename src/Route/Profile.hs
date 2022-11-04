
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import Data.HashMap.Strict as HS
import Data.Int
import Data.Text.Lazy as TL
import GHC.Generics
import Data.Aeson
import Data.Time
import Data.String (IsString)
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

data ProfilePostData = ProfilePostData {
    birthDate :: Day
  , gender    :: TL.Text
  , height    :: Double
} deriving (Eq, Show, Generic, ToJSON, FromJSON, FromForm)

data ProfileEditData = ProfileEditData {
    editBirthDate :: Day
  , editGender    :: Bool
  , editHeight    :: Double
} deriving (Eq, Show, Generic, ToJSON, FromJSON, FromForm)

data ProfileGetContext = ProfileGetContext {
    profileAccountName :: TL.Text
  , profileId          :: Int32
  , profileGender      :: TL.Text
  , profileBirthDate   :: Day
} deriving (Generic, Show, ToJSON, FromJSON)


type ProfileAPI =  "profile" :> ( AuthProtect "cookie-auth"
                                    :> UVerb 'GET '[HTML] [ WithStatus 403 RawHtml
                                                          , WithStatus 404 RawHtml
                                                          , WithStatus 200 RawHtml
                                                          ]
                             :<|> AuthProtect "cookie-auth"
                                    :> "form"
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
                                    :> Capture "profileId" Int32
                                    :> "form"
                                    :> UVerb 'GET '[HTML] [ WithStatus 403 RawHtml
                                                          , WithStatus 404 RawHtml
                                                          , WithStatus 200 RawHtml
                                                          ]
                            --  :<|> AuthProtect "cookie-auth"
                            --         :> Capture "profileId" Int32
                            --         :> "put"
                            --         :> ReqBody '[FormUrlEncoded] ProfilePostData
                            --         :> UVerb 'PUT '[HTML] [ WithStatus 403 RawHtml
                            --                               , WithStatus 404 RawHtml
                            --                               , WithStatus 303 ( Headers '[ Header "Location" RedirectUrl
                            --                                                           ] NoContent )
                            --                               ]
                                )

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
            -- :<|> profilePutHandler

  where profileGetHandler :: Maybe SignInAccount
                          -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                    , WithStatus 404 RawHtml
                                                    , WithStatus 200 RawHtml ] )
        profileGetHandler Nothing = respond =<< liftIO authFailToSignInView
        profileGetHandler (Just account) = do
          pool <- asks getPool
          maybeProfile <- liftIO $ selectProfile pool account
          case maybeProfile of
            Nothing -> do html <- TP.htmlHandler context "/profile_form.html"
                          respond $ WithStatus @404 $ html
              where
                    context = HS.fromList [ ( "globalMsgs"
                                            , toJSON ["Haven't created an account." :: Text] ) ]
            Just profile -> do html <- TP.htmlHandler context "/profile.html"
                               respond $ WithStatus @200 $ html
              where
                    context = HS.fromList [ ( "ctx"
                                            , toJSON $ ProfileGetContext
                                                         (accountName account)
                                                         (_profileId profile)
                                                         (bool "女" "男" $ _profileGender profile)
                                                         (_profileBirthDate profile) ) ]
          where
                selectProfile pool account =
                  withResource pool $ \conn -> runBeamPostgres conn $
                  runSelectReturningOne $ select $
                  filter_ (((AccountId . val_ . accountId) account ==.) . _profileAccountId) $
                  all_ $ _healthProfile healthDb

        profileGetCreateFormHandler :: Maybe SignInAccount
                                    -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                              , WithStatus 200 RawHtml ] )
        profileGetCreateFormHandler Nothing        = respond =<< liftIO authFailToSignInView
        profileGetCreateFormHandler (Just account) = do
          html <- TP.htmlHandler mempty "/profile_form.html"
          respond $ WithStatus @200 $ html

        profilePostHandler :: (ToHttpApiData RedirectUrl, IsString RedirectUrl) =>
             Maybe SignInAccount
          -> ProfilePostData
          -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                    , WithStatus 303 (Headers '[ Header "Location" RedirectUrl] NoContent ) ] )
        profilePostHandler Nothing _ = respond =<< liftIO authFailToSignInView
        profilePostHandler (Just account) profileFormData = do
          pool <- asks getPool
          profile <- liftIO $ insertProfile pool account profileFormData
          red <- redirect ("/auth/sign_in" :: RedirectUrl)
          respond $ WithStatus @303 $ red

            where
                  insertProfile :: Pool Connection -> SignInAccount -> ProfilePostData -> IO Profile
                  insertProfile pool account profileFormData = do
                    [profile] <- withResource pool $ \conn -> runBeamPostgres conn $
                      runInsertReturningList $ Database.Beam.insert (_healthProfile healthDb) $
                      insertExpressions [ Profile { _profileId         = default_
                                                  , _profileAccountId  = AccountId $ val_ $ accountId account
                                                  , _profileGender     = val_ $ bool False True $
                                                                                gender profileFormData == "male"
                                                  , _profileBirthDate  = val_ $ birthDate profileFormData
                                                  , _profileHeight = val_ $ height profileFormData
                                                  } ]
                    pure profile

        profileGetEditFormHandler :: Maybe SignInAccount
                                  -> Int32
                                  -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                            , WithStatus 404 RawHtml
                                                            , WithStatus 200 RawHtml ] )
        profileGetEditFormHandler Nothing _ = respond =<< liftIO authFailToSignInView
        profileGetEditFormHandler (Just account) profileId = do
          pool <- asks getPool
          profile <- liftIO $ selectProfile pool account
          let authorizedProfile = profile >>= checkHasProfilePermission account
          case authorizedProfile of
            Left (ServerError 404 _ errBody _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @404 $ html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 errBody] )]
            Left (ServerError 403 _ errBody _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @403 $ html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 errBody] )]
            Right profile -> do
              html <- TP.htmlHandler context "/profile_form_edit.html"
              respond $ WithStatus @200 $ html
              where context = HS.fromList [ ( "profileId", toJSON $ _profileId profile )
                                          , ( "profileData", toJSON $ ProfileEditData (_profileBirthDate profile)
                                                                                      (_profileGender profile)
                                                                                      (_profileHeight profile) )
                                          ]
            _ -> throw err500
          where
                selectProfile :: Pool Connection -> SignInAccount -> IO (Either ServerError Profile)
                selectProfile pool signInAccount = do
                  profile <- withResource pool $ \conn -> runBeamPostgres conn $
                    runSelectReturningOne $ select $ do
                      account <- all_ $ _healthAccount healthDb
                      profile <- join_ (_healthProfile healthDb) $ (primaryKey account ==.) . _profileAccountId
                      guard_ $ _accountId account ==. val_ (accountId signInAccount)
                      pure profile
                  pure $ maybeToRight err404 { errBody = TLE.encodeUtf8 "檔案不存在"} profile

                checkHasProfilePermission :: SignInAccount -> Profile -> Either ServerError Profile
                checkHasProfilePermission signInAccount profile
                  | _profileAccountId profile == (AccountId . accountId) signInAccount = Right profile
                  | otherwise = Left err403 { errBody = TLE.encodeUtf8 "你沒有權限進行編輯" }

        -- TODO profile put api
        -- profilePutHandler
