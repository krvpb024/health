
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
import Data.Fixed
import Data.Scientific

data ProfileData = ProfileData {
    birthDate  :: Day
  , gender     :: Text
  , initHeight :: Double
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, FromForm)

data ProfileGetContext = ProfileGetContext {
    profileAccountName :: TL.Text
  , profileGender      :: Text
  , profileBirthDate   :: Day
  } deriving (Generic, Show, ToJSON, FromJSON)


type ProfileAPI =  "profile" :> ( AuthProtect "cookie-auth" :> UVerb 'GET '[HTML] [ WithStatus 401 RawHtml
                                                                                  , WithStatus 404 RawHtml
                                                                                  , WithStatus 200 RawHtml
                                                                                  ]
                             :<|> AuthProtect "cookie-auth" :> "form"
                                    :> UVerb 'GET '[HTML] [ WithStatus 401 RawHtml
                                                          , WithStatus 200 RawHtml
                                                          ]
                             :<|> AuthProtect "cookie-auth" :> ReqBody '[FormUrlEncoded] ProfileData
                                    :> UVerb 'POST '[HTML] [ WithStatus 401 RawHtml
                                                           , WithStatus 303 ( Headers '[ Header "Location" RedirectUrl
                                                                                       ] NoContent )
                                                           ]
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
            :<|> profileFormGetHandler
            :<|> profilePostHandler

  where profileGetHandler :: Maybe SignInAccount
                          -> ReaderHandler ( Union '[ WithStatus 401 RawHtml
                                                    , WithStatus 404 RawHtml
                                                    , WithStatus 200 RawHtml ] )
        profileGetHandler Nothing = do
          html <- TP.htmlHandler context "/sign_in.html"
          respond $ WithStatus @401 $ html
          where
                context = HS.fromList [ authFailHandlerMessage ]
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
                                                         (bool "女" "男" $ _profileGender profile)
                                                         (_profileBirthDate profile) ) ]
          where
                selectProfile pool account =
                  withResource pool $ \conn -> runBeamPostgres conn $
                  runSelectReturningOne $ select $
                  filter_ (((AccountId . val_ . accountId) account ==.) . _profileAccountId) $
                  all_ $ _healthProfile healthDb

        profileFormGetHandler :: Maybe SignInAccount
                              -> ReaderHandler ( Union '[ WithStatus 401 RawHtml
                                                        , WithStatus 200 RawHtml ] )
        profileFormGetHandler Nothing        = do html <- TP.htmlHandler context "/sign_in.html"
                                                  respond $ WithStatus @401 $ html
          where
                context = HS.fromList [ authFailHandlerMessage ]
        profileFormGetHandler (Just account) = do
          html <- TP.htmlHandler mempty "/profile_form.html"
          respond $ WithStatus @200 $ html

        profilePostHandler :: (ToHttpApiData RedirectUrl, IsString RedirectUrl) =>
             Maybe SignInAccount
          -> ProfileData
          -> ReaderHandler ( Union '[ WithStatus 401 RawHtml
                                    , WithStatus 303 (Headers '[ Header "Location" RedirectUrl] NoContent ) ] )
        profilePostHandler Nothing _ = do html <- TP.htmlHandler context "/sign_in.html"
                                          respond $ WithStatus @401 $ html
          where
                context = HS.fromList [ authFailHandlerMessage ]

        profilePostHandler (Just account) profileFormData = do
          pool <- asks getPool
          profile <- liftIO $ insertProfile pool account profileFormData
          red <- redirect ("/auth/sign_in" :: RedirectUrl)
          respond $ WithStatus @303 $ red

            where
                  insertProfile :: Pool Connection -> SignInAccount -> ProfileData -> IO Profile
                  insertProfile pool account profileFormData = do
                    [profile] <- withResource pool $ \conn -> runBeamPostgres conn $
                      runInsertReturningList $ Database.Beam.insert (_healthProfile healthDb) $
                      insertExpressions [ Profile { _profileId         = default_
                                                  , _profileAccountId  = AccountId $ val_ $ accountId account
                                                  , _profileGender     = val_ $ bool False True $
                                                                                gender profileFormData == "male"
                                                  , _profileBirthDate  = val_ $ birthDate profileFormData
                                                  , _profileInitHeight = val_ $ fromFloatDigits $ initHeight profileFormData
                                                  } ]
                    pure profile
