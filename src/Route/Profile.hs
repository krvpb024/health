{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Text.Lazy.Encoding as TLE
import Data.Int
import Data.Text.Lazy as TL
import GHC.Generics
import Data.Aeson

type ProfileAPI = "profile" :> AuthProtect "cookie-auth" :> Get '[HTML] RawHtml

profileAPI :: Proxy ProfileAPI
profileAPI = Proxy

profileServerReader :: Reader Env (Server ProfileAPI)
profileServerReader = asks $ \env ->
  hoistServerWithContext profileAPI
                         (Proxy :: Proxy '[AuthHandler Request (Maybe SignInAccount)])
                         (readerToHandler env)
                         profileServerT

data ProfileGetContext = ProfileGetContext { profileAccountId :: Int32
                                           , profileAccountName :: TL.Text
                                           } deriving (Generic, Show)
instance ToJSON ProfileGetContext
instance FromJSON ProfileGetContext

profileServerT :: ServerT ProfileAPI ReaderHandler
profileServerT = profileGetHandler

  where profileGetHandler :: Maybe SignInAccount
                          -> ReaderHandler RawHtml
        profileGetHandler Nothing        = TP.htmlHandler mempty "/sign_up.html"
        profileGetHandler (Just account) = do
          pool <- asks getPool
          TP.htmlHandler context "/profile.html"
          where context = HS.fromList [ ( "ctx"
                                         , toJSON $ ProfileGetContext (accountId account)
                                                                      (accountName account))
                                      ]
