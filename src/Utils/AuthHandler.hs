{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Utils.AuthHandler where

import Servant
import Servant.Server.Experimental.Auth
import Network.Wai
import Web.Cookie
import Network.HTTP.Types.Header as H
import Data.Maybe
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Conf
import Database.Beam.Postgres
import Data.Pool
import Control.Monad.Cont
import Database.Beam
import Schema
import Data.Text.Lazy as TL
import Data.Int
import Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding as TSE

type instance AuthServerData (AuthProtect "cookie-auth") = Maybe SignInAccount

data SignInAccount = SignInAccount { sessionId :: Text
                                   , accountId :: Int32
                                   , accountName :: Text
                                   } deriving (Show)

lookupAccount :: Pool Connection -> Maybe Text -> Handler (Maybe SignInAccount)
lookupAccount pool sid = do
  sessionJoinedAccount <- liftIO $ lookupAccount' pool sid
  return $ sessionJoinedAccount >>=
            \(s, a) -> Just $ SignInAccount (_sessionId s)
                                            (_accountId a)
                                            (_accountName a)

lookupAccount' :: Pool Connection
               -> Maybe Text
               -> IO (Maybe (SessionT Identity, AccountT Identity))
lookupAccount' _    Nothing          = return Nothing
lookupAccount' pool (Just sessionId) = do
  withResource pool $ \conn -> runBeamPostgres conn $
    runSelectReturningOne $ select $ do
      account <- all_ $ _healthAccount healthDb
      joinedSessionAccount <- join_ (_healthSession healthDb) $ (primaryKey account ==.) . _sessionAccountId
      guard_ $ _sessionId joinedSessionAccount ==. val_ sessionId
      pure (joinedSessionAccount, account)

authHandler :: Env -> AuthHandler Request (Maybe SignInAccount)
authHandler env = mkAuthHandler handler
  where handler req = lookupAccount pl sid
          where headers = requestHeaders req
                cookie = lookup ("cookie" :: HeaderName) headers
                sid = fmap (TL.fromStrict . TSE.decodeUtf8) $ lookup "servant-auth-cookie" $ parseCookies $ fromMaybe "" cookie
                pl = getPool env

genAuthServerContext :: Env -> Context (AuthHandler Request (Maybe SignInAccount) ': '[])
genAuthServerContext env = authHandler env :. EmptyContext
