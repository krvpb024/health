{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Utils.AuthHandler where

import Servant
import Servant.Server.Experimental.Auth
import Network.Wai
import Web.Cookie
import Network.HTTP.Types.Header
import Data.Maybe
import Conf
import Database.Beam.Postgres
import Data.Pool
import Control.Monad.Cont
import Database.Beam
import Schema
import Data.Text.Lazy as TL
import Data.Int
import Data.Text.Encoding as TSE
import Data.Time
import Text.Ginger
import Data.Aeson
import qualified Utils.TemplateHandler as TP
import qualified Data.HashMap.Strict as HS

type instance AuthServerData (AuthProtect "cookie-auth") = Maybe SignInAccount

data SignInAccount = SignInAccount { sessionId :: Text
                                   , accountId :: Int32
                                   , accountName :: Text
                                   } deriving (Show)

lookupAccount :: Pool Connection
              -> Maybe Text
              -> Handler (Maybe SignInAccount)
lookupAccount pool sid = do
  sessionJoinedAccount <- liftIO $ lookupAccount' pool sid
  return $ sessionJoinedAccount >>=
            \(s, a) -> Just $ SignInAccount (_sessionId s)
                                            (_accountId a)
                                            (_accountName a)

  where lookupAccount' :: Pool Connection
                       -> Maybe Text
                       -> IO (Maybe (SessionT Identity, AccountT Identity))
        lookupAccount' _    Nothing          = return Nothing
        lookupAccount' pool (Just sessionId) = do
          currentTimestamp <- getZonedTime
          selectSession pool sessionId currentTimestamp

          where selectSession :: Pool Connection
                              -> Text
                              -> ZonedTime
                              -> IO (Maybe (SessionT Identity, AccountT Identity))
                selectSession pool sessionId currentTimestamp =
                  withResource pool $ \conn -> runBeamPostgres conn $
                  runSelectReturningOne $ select $ do
                    account <- all_ $ _healthAccount healthDb
                    session <- join_ (_healthSession healthDb) $ (primaryKey account ==.) . _sessionAccountId
                    guard_ $ (_sessionId session ==. val_ sessionId) &&.
                             (_sessionExpireAt session >. val_ (zonedTimeToLocalTime currentTimestamp))
                    pure (session, account)

authHandler :: Env -> AuthHandler Request (Maybe SignInAccount)
authHandler env = mkAuthHandler handler
  where handler req = lookupAccount pool sid
          where headers = requestHeaders req
                sid = TL.fromStrict . TSE.decodeUtf8 <$>
                      lookup "servant-auth-cookie"
                             (parseCookies $ fromMaybe mempty cookie)
                cookie = lookup ("cookie" :: HeaderName) headers
                pool = getPool env

genAuthServerContext :: Env -> Context (AuthHandler Request (Maybe SignInAccount) ': '[])
genAuthServerContext env = authHandler env :. EmptyContext

authFailHandlerMessage :: (VarName, Value)
authFailHandlerMessage = ( "globalMsgs", toJSON ["You Need to Sign In First." :: Text] )
