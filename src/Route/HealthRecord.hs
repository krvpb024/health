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

type HealthRecordAPI = "health_record" :> ( AuthProtect "cookie-auth" :> UVerb 'GET '[HTML] [ WithStatus 401 RawHtml
                                                                                            , WithStatus 200 RawHtml
                                                                                            ]
                                       :<|> AuthProtect "cookie-auth" :> Capture "foo" Int32
                                              :> UVerb 'GET '[HTML] [ WithStatus 401 RawHtml
                                                                    , WithStatus 404 RawHtml
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
      height            :: Int32
    , weight            :: Scientific
    , bodyFatPercentage :: Maybe Scientific
    , waistlineCm       :: Maybe Scientific
    , recordAt          :: String
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

healthRecordServerT :: ServerT HealthRecordAPI ReaderHandler
healthRecordServerT = healthRecordListGetHandler
                 :<|> healthRecordGetHandler

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
                selectHealthRecordList :: Pool Connection -> SignInAccount -> IO [(Profile, HealthRecord)]
                selectHealthRecordList pool signInAccount = do
                  withResource pool $ \conn -> runBeamPostgres conn $
                    runSelectReturningList $ select $ orderBy_ (desc_ . _healthRecordRecordAt . snd) $ do
                      account <- all_ $ _healthAccount healthDb
                      profile <- join_ (_healthProfile healthDb) $ (primaryKey account ==.) . _profileAccountId
                      healthRecord <- join_ (_healthHealthRecord healthDb) $ (primaryKey profile ==.) . _healthRecordProfileId
                      guard_ $ _accountId account ==. val_ (accountId signInAccount)
                      pure (profile, healthRecord)

                computeBmiAndOthers :: (Profile, HealthRecord) -> HealthRecordWithComputedValue
                computeBmiAndOthers (profile, healthRecord) =
                  HealthRecordWithComputedValue {
                      height = _healthRecordHeight healthRecord
                    , weight = _healthRecordWeight healthRecord
                    , bodyFatPercentage = _healthRecordBodyFatPercentage healthRecord
                    , waistlineCm = _healthRecordWaistlineCm healthRecord
                    , recordAt = formatTime
                                  defaultTimeLocale
                                  "%F"
                                  (_healthRecordRecordAt healthRecord)
                  }

        -- FIXME just match type
        healthRecordGetHandler :: Maybe SignInAccount
                               -> Int32
                               -> ReaderHandler ( Union '[ WithStatus 401 RawHtml
                                                         , WithStatus 404 RawHtml
                                                         , WithStatus 200 RawHtml ] )
        healthRecordGetHandler Nothing _ = do
          html <- TP.htmlHandler context "/sign_in.html"
          respond $ WithStatus @401 $ html
          where
                context = HS.fromList [ authFailHandlerMessage ]
        healthRecordGetHandler (Just account) recordId = do
          let i = Nothing
          case i of
            Nothing -> do
              html <- TP.htmlHandler context "/sign_in.html"
              respond $ WithStatus @404 $ html
            Just _ -> do
              html <- TP.htmlHandler context "/sign_in.html"
              respond $ WithStatus @200 $ html
          where
                context = HS.fromList [ authFailHandlerMessage ]
