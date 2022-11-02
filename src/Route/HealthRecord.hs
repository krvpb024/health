{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Route.HealthRecord where
import Data.Data
import Control.Monad.Trans.Reader
import Conf
import Servant
import Route.Profile
import Servant.Server.Experimental.Auth
import Network.Wai
import Utils.AuthHandler
import Utils.ReaderHandler
import Utils.TemplateHandler as TP
import qualified Data.HashMap.Strict as HS
import Data.Int
import Schema
import Data.Pool
import Database.Beam.Postgres
import Database.Beam
import Data.Scientific
import Data.Time
import Data.Aeson
import Text.Ginger
import Data.Either.Combinators
import Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Fixed

type HealthRecordAPI = "health_record" :> ( AuthProtect "cookie-auth" :> UVerb 'GET '[HTML] [ WithStatus 401 RawHtml
                                                                                            , WithStatus 200 RawHtml
                                                                                            ]
                                       :<|> AuthProtect "cookie-auth" :> "form"
                                              :> UVerb 'GET '[HTML] [ WithStatus 401 RawHtml
                                                                    , WithStatus 403 RawHtml
                                                                    , WithStatus 200 RawHtml
                                                                    ]
                                       :<|> AuthProtect "cookie-auth" :> Capture "recordId" Integer
                                              :> "put"
                                              :> UVerb 'GET '[HTML] [ WithStatus 401 RawHtml
                                                                    , WithStatus 404 RawHtml
                                                                    , WithStatus 403 RawHtml
                                                                    , WithStatus 200 RawHtml
                                                                    ]
                                          )

healthRecordAPI :: Proxy HealthRecordAPI
healthRecordAPI = Proxy

healthRecordServerReader :: Reader Env (Server HealthRecordAPI)
healthRecordServerReader = asks $ \env ->
  hoistServerWithContext healthRecordAPI
                        (Proxy :: Proxy '[AuthHandler Request (Maybe SignInAccount)])
                        (readerToHandler env)
                        healthRecordServerT

data HealthRecordWithComputedValue =
  HealthRecordWithComputedValue {
      recordId          :: Integer
    , height            :: Scientific
    , weight            :: Scientific
    , bmi               :: Centi
    , bodyFatPercentage :: Maybe Scientific
    , waistlineCm       :: Maybe Scientific
    , recordDate        :: Day
    , recordAt          :: String
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

healthRecordServerT :: ServerT HealthRecordAPI ReaderHandler
healthRecordServerT = healthRecordListGetHandler
                 :<|> healthRecordPostFormHandler
                 :<|> healthRecordPutHandler

  where
        healthRecordListGetHandler :: Maybe SignInAccount
                                   -> ReaderHandler ( Union '[ WithStatus 401 RawHtml
                                                             , WithStatus 200 RawHtml ] )
        healthRecordListGetHandler Nothing = do
          html <- TP.htmlHandler context "/sign_in.html"
          respond $ WithStatus @401 $ html
          where
                context = HS.fromList [ authFailHandlerMessage ]
        healthRecordListGetHandler (Just account) = do
          pool <- asks getPool
          joinedHealthRecordList <- liftIO $ selectHealthRecordList pool account
          let healthRecordWithComputedValueList = computeBmiAndOthers <$> joinedHealthRecordList
              context = HS.fromList [ ( "healthRecordList"
                                      , toJSON (healthRecordWithComputedValueList :: [HealthRecordWithComputedValue])) ]
          html <- TP.htmlHandler context "/health_record_list.html"
          respond $ WithStatus @200 $ html
          where
                selectHealthRecordList :: Pool Connection
                                       -> SignInAccount
                                       -> IO [(Profile, HealthRecord)]
                selectHealthRecordList pool signInAccount = do
                  withResource pool $ \conn -> runBeamPostgres conn $
                    runSelectReturningList $ select $ orderBy_ (desc_ . _healthRecordDate . snd) $ do
                      account <- all_ $ _healthAccount healthDb
                      profile <- join_ (_healthProfile healthDb) $ (primaryKey account ==.) . _profileAccountId
                      healthRecord <- join_ (_healthHealthRecord healthDb) $ (primaryKey profile ==.) . _healthRecordProfileId
                      guard_ $ _accountId account ==. val_ (accountId signInAccount)
                      pure (profile, healthRecord)

                computeBmiAndOthers :: (Profile, HealthRecord) -> HealthRecordWithComputedValue
                computeBmiAndOthers (profile, healthRecord) =
                  HealthRecordWithComputedValue {
                      recordId = _healthRecordId healthRecord
                    , height = _healthRecordHeight healthRecord
                    , weight = _healthRecordWeight healthRecord
                    , bmi = realToFrac bmiValue :: Centi
                    , bodyFatPercentage = _healthRecordBodyFatPercentage healthRecord
                    , waistlineCm = _healthRecordWaistlineCm healthRecord
                    , recordDate = _healthRecordDate healthRecord
                    , recordAt = formatTime
                                  defaultTimeLocale
                                  "%F"
                                  (_healthRecordRecordAt healthRecord)
                  }
                  where w = toRealFloat (_healthRecordWeight healthRecord)
                        h = toRealFloat (_healthRecordHeight healthRecord) / 100
                        bmiValue = w / h / h :: Double

        -- FIXME healthRecordPostFormHandler
        healthRecordPostFormHandler :: Maybe SignInAccount
                                    -> ReaderHandler ( Union '[ WithStatus 401 RawHtml
                                                              , WithStatus 403 RawHtml
                                                              , WithStatus 200 RawHtml ] )
        healthRecordPostFormHandler Nothing = do
          html <- TP.htmlHandler context "/sign_in.html"
          respond $ WithStatus @401 $ html
          where
                context = HS.fromList [ authFailHandlerMessage ]
        healthRecordPostFormHandler (Just account) = do
          pool <- asks getPool
          currentTime <- liftIO getZonedTime
          profile <- liftIO $ selectProfile pool account
          case profile of
            Left err -> do
              html <- TP.htmlHandler context "/sign_in.html"
              respond $ WithStatus @403 $ html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 $ errBody err] )]
            Right profile -> do
              html <- TP.htmlHandler context "/health_record_form.html"
              respond $ WithStatus @200 $ html
              where context = HS.fromList [ ( "currentDate"
                                              , toJSON $ formatTime defaultTimeLocale "%F" currentTime )
                                          , ( "initHeight"
                                              , toJSON $ _profileInitHeight profile ) ]

          where
                selectProfile :: Pool Connection -> SignInAccount -> IO (Either ServerError Profile)
                selectProfile pool signInAccount = do
                  profile <- withResource pool $ \conn -> runBeamPostgres conn $
                    runSelectReturningOne $ select $ do
                      account <- all_ $ _healthAccount healthDb
                      profile <- join_ (_healthProfile healthDb) $ (primaryKey account ==.) . _profileAccountId
                      guard_ $ _accountId account ==. val_ (accountId signInAccount)
                      pure profile
                  pure $ maybeToRight err403 { errBody = "你必須先建立個人檔案"} profile
        -- FIXME healthRecordPostHandler

        -- FIXME healthRecordPutFormHandler

        -- FIXME healthRecordPutHandler just match type
        healthRecordPutHandler :: Maybe SignInAccount
                               -> Integer
                               -> ReaderHandler ( Union '[ WithStatus 401 RawHtml
                                                         , WithStatus 404 RawHtml
                                                         , WithStatus 403 RawHtml
                                                         , WithStatus 200 RawHtml ] )
        healthRecordPutHandler Nothing _ = do
          html <- TP.htmlHandler context "/sign_in.html"
          respond $ WithStatus @401 $ html
          where
                context = HS.fromList [ authFailHandlerMessage ]
        healthRecordPutHandler (Just account) recordId = do
          pool <- asks getPool
          record <- liftIO $ selectHealthRecord pool recordId account
          let authorizedRecord = record >>= checkRecordPermission account
          case authorizedRecord of
            Left err -> do
              html <- TP.htmlHandler context "/sign_in.html"
              respond $ WithStatus @403 $ html
              where context :: HS.HashMap VarName Value
                    context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 $ errBody err] )]
            Right _ -> do
              html <- TP.htmlHandler context "/sign_in.html"
              respond $ WithStatus @200 $ html
          where
                context = HS.fromList [ authFailHandlerMessage ]
                e404 = err404 { errBody = "找不到這筆記錄"}
                e403 = err403 { errBody = "你沒有權限修改這筆記錄"}

                selectHealthRecord :: Pool Connection
                                   -> Integer
                                   -> SignInAccount
                                   -> IO (Either ServerError (Account, HealthRecord))
                selectHealthRecord pool recordId signInAccount =
                  withResource pool $ \conn -> runBeamPostgres conn $ do
                    accountAndRecord <- runSelectReturningOne $ select $ do
                      account <- all_ $ _healthAccount healthDb
                      profile <- join_ (_healthProfile healthDb) $ (primaryKey account ==.) . _profileAccountId
                      healthRecord <- join_ (_healthHealthRecord healthDb) $ (primaryKey profile ==.) . _healthRecordProfileId
                      guard_ $ _healthRecordId healthRecord ==. val_ recordId
                      pure (account, healthRecord)
                    pure $ maybeToRight e404 accountAndRecord

                checkRecordPermission :: SignInAccount
                                      -> (Account, HealthRecord)
                                      -> Either ServerError HealthRecord
                checkRecordPermission signInAccount (account, record)
                  | _accountId account == accountId signInAccount = Right record
                  | otherwise                                     = Left e403


