{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Route.Profile where

import Servant
import Utils.TemplateHandler
import Utils.ReaderHandler
import Control.Monad.Reader
import Conf
import Utils.AuthHandler
import Servant.Server.Experimental.Auth
import Network.Wai
import Data.HashMap.Strict
import Data.Text
import Text.Ginger
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Text.Lazy.Encoding as TLE

type ProfileAPI = "profile" :> AuthProtect "cookie-auth" :> Get '[HTML] RawHtml

profileAPI :: Proxy ProfileAPI
profileAPI = Proxy

profileServerReader :: Reader Env (Server ProfileAPI)
profileServerReader = asks $ \env ->
  hoistServerWithContext profileAPI (Proxy :: Proxy '[AuthHandler Request (Maybe SignInAccount)]) (readerToHandler env) profileServerT

profileServerT :: ServerT ProfileAPI ReaderHandler
profileServerT = profileGetHandler

  where profileGetHandler :: Maybe SignInAccount -> ReaderHandler RawHtml
        profileGetHandler Nothing        = Utils.TemplateHandler.htmlHandler mempty "/sign_up.html"
        profileGetHandler (Just account) = do
          pool <- asks getPool
          liftIO $ print account
          let context :: HashMap VarName BL.ByteString
              context = fromList [ ("accountId", BLU.fromString $ show $ accountId account)
                                 , ("accountName", encodeUtf8 $ accountName account)
                                 ]
          Utils.TemplateHandler.htmlHandler context "/profile.html"
