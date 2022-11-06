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
import Servant.Server.Experimental.Auth
import Network.Wai
import Utils.AuthHandler
import Utils.ReaderHandler
import Utils.TemplateHandler as TP
import qualified Data.HashMap.Strict as HS
import Schema
import Data.Pool
import Database.Beam.Postgres
import Database.Beam
import Data.Time
import Data.Aeson
import Data.Either.Combinators
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Fixed
import Web.FormUrlEncoded
import Database.Beam.Backend.SQL.BeamExtensions
import Data.Int
import Control.Exception

type HealthRecordAPI = "health_record" :> ( AuthProtect "cookie-auth"
                                              :> UVerb 'GET '[HTML] [ WithStatus 403 RawHtml
                                                                    , WithStatus 200 RawHtml
                                                                    ]
                                       :<|> AuthProtect "cookie-auth"
                                              :> "form"
                                              :> UVerb 'GET '[HTML] [ WithStatus 403 RawHtml
                                                                    , WithStatus 200 RawHtml
                                                                    ]
                                       :<|> AuthProtect "cookie-auth"
                                              :> ReqBody '[FormUrlEncoded] PostHealthRecordData
                                              :> UVerb 'POST '[HTML] [ WithStatus 403 RawHtml
                                                                     , WithStatus 303 (Headers '[ Header "Location" RedirectUrl] NoContent )
                                                                     ]
                                       :<|> AuthProtect "cookie-auth"
                                              :> Capture "recordId" Int32
                                              :> "delete"
                                              :> UVerb 'POST '[HTML] [ WithStatus 403 RawHtml
                                                                     , WithStatus 404 RawHtml
                                                                     , WithStatus 303 (Headers '[ Header "Location" RedirectUrl] NoContent )
                                                                     ]
                                       :<|> AuthProtect "cookie-auth"
                                              :> Capture "recordId" Int32
                                              :> "form"
                                              :> UVerb 'GET '[HTML] [ WithStatus 403 RawHtml
                                                                    , WithStatus 404 RawHtml
                                                                    , WithStatus 200 RawHtml
                                                                    ]
                                       :<|> AuthProtect "cookie-auth"
                                              :> Capture "recordId" Int32
                                              :> "put"
                                              :> ReqBody '[FormUrlEncoded] PostHealthRecordData
                                              :> UVerb 'POST '[HTML] [ WithStatus 403 RawHtml
                                                                     , WithStatus 404 RawHtml
                                                                     , WithStatus 303 (Headers '[ Header "Location" RedirectUrl] NoContent )
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

data HealthRecordTimeFilterMode =
    PastWeek
  | PastMonth
  | PastNDay Integer
  | Between UTCTime UTCTime
  deriving (Eq, Show, Generic, ToJSON, FromJSON, FromForm)

data PostHealthRecordData = PostHealthRecordData {
    postHeight            :: Double
  , postWeight            :: Double
  , postBodyFatPercentage :: Maybe Double
  , postWaistlineCm       :: Maybe Double
  , postDate              :: Day
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromForm PostHealthRecordData where
  fromForm f = PostHealthRecordData
    <$> parseUnique "postHeight" f
    <*> parseUnique "postWeight" f
    <*> (optionalDoubleToMaybe <$> parseUnique "postBodyFatPercentage" f)
    <*> (optionalDoubleToMaybe <$> parseUnique "postWaistlineCm" f)
    <*> parseUnique "postDate" f

    where optionalDoubleToMaybe :: String -> Maybe Double
          optionalDoubleToMaybe "" = Nothing
          optionalDoubleToMaybe s = Just $ read s

data HealthRecordWithComputedValue = HealthRecordWithComputedValue {
    recordId          :: Int32
  , height            :: Double
  , weight            :: Double
  , bmi               :: Fixed E1
  , bodyFatPercentage :: Maybe Double
  , waistlineCm       :: Maybe Double
  , recordDate        :: Day
  , recordAt          :: String
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

healthRecordServerT :: ServerT HealthRecordAPI ReaderHandler
healthRecordServerT = healthRecordListGetHandler
                 :<|> healthRecordGetCreateFormHandler
                 :<|> healthRecordPostHandler
                 :<|> healthRecordDeleteHandler
                 :<|> healthRecordGetEditFormHandler
                 :<|> healthRecordPutHandler

  where
        healthRecordListGetHandler :: Maybe SignInAccount
                                   -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                             , WithStatus 200 RawHtml ] )
        healthRecordListGetHandler Nothing = respond =<< liftIO authFailToSignInView
        healthRecordListGetHandler (Just account) = do
          pool <- asks getPool
          joinedHealthRecordList <- liftIO $ selectHealthRecordList pool account
          let healthRecordWithComputedValueList = computeBmiAndOthers <$> joinedHealthRecordList
              context = HS.fromList [ ( "healthRecordList"
                                      , toJSON (healthRecordWithComputedValueList :: [HealthRecordWithComputedValue])) ]
          html <- TP.htmlHandler context "/health_record_list.html"
          respond $ WithStatus @200 html
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
                    , bmi = realToFrac bmiValue :: Fixed E1
                    , bodyFatPercentage = _healthRecordBodyFatPercentage healthRecord
                    , waistlineCm = _healthRecordWaistlineCm healthRecord
                    , recordDate = _healthRecordDate healthRecord
                    , recordAt = formatTime
                                  defaultTimeLocale
                                  "%F"
                                  (_healthRecordRecordAt healthRecord)
                  }
                  where w = _healthRecordWeight healthRecord
                        h = _healthRecordHeight healthRecord / 100
                        bmiValue = w / h / h :: Double

        healthRecordGetCreateFormHandler :: Maybe SignInAccount
                                    -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                              , WithStatus 200 RawHtml ] )
        healthRecordGetCreateFormHandler Nothing = respond =<< liftIO authFailToSignInView
        healthRecordGetCreateFormHandler (Just account) = do
          pool <- asks getPool
          profile <- liftIO $ selectProfile pool account
          case profile of
            Left err -> do
              html <- TP.htmlHandler context "/sign_in.html"
              respond $ WithStatus @403 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 $ errBody err] )]
            Right profile -> do
              currentTime <- liftIO getZonedTime
              let context = HS.fromList [ ( "currentDate"
                                              , toJSON $ formatTime defaultTimeLocale "%F" currentTime )
                                          , ( "height"
                                              , toJSON $ _profileHeight profile ) ]
              html <- TP.htmlHandler context "/health_record_form.html"
              respond $ WithStatus @200 html

        healthRecordPostHandler :: Maybe SignInAccount
                                -> PostHealthRecordData
                                -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                          , WithStatus 303 ( Headers '[ Header "Location" RedirectUrl] NoContent ) ] )
        healthRecordPostHandler Nothing _ = respond =<< liftIO authFailToSignInView
        healthRecordPostHandler (Just account) postHealthRecordData = do
          pool <- asks getPool
          profile <- liftIO $ selectProfile pool account
          case profile of
            Left err -> do
              html <- TP.htmlHandler context "/sign_in.html"
              respond $ WithStatus @403 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 $ errBody err] )]

            Right profile -> do
              healthRecordId <- liftIO $ insertHealthRecord pool postHealthRecordData profile
              red <- redirect ("/health_record" :: RedirectUrl)
              respond $ WithStatus @303 red

            where
                  insertHealthRecord :: Pool Connection
                                     -> PostHealthRecordData
                                     -> Profile
                                     -> IO Int32
                  insertHealthRecord pool postHealthRecordData profile = do
                    [healthRecord] <- withResource pool $ \conn -> runBeamPostgres conn $
                      runInsertReturningList $ insert (_healthHealthRecord healthDb) $
                      insertExpressions [ HealthRecord {
                          _healthRecordId = default_
                        , _healthRecordProfileId = val_ $ primaryKey profile
                        , _healthRecordHeight = val_ $ postHeight postHealthRecordData
                        , _healthRecordWeight = val_ $ postWeight postHealthRecordData
                        , _healthRecordBodyFatPercentage = val_ $ postBodyFatPercentage postHealthRecordData
                        , _healthRecordWaistlineCm = val_ $ postWaistlineCm postHealthRecordData
                        , _healthRecordDate = val_ $ postDate postHealthRecordData
                        , _healthRecordRecordAt = default_
                      } ]
                    pure (_healthRecordId healthRecord)

        healthRecordDeleteHandler :: Maybe SignInAccount
                                  -> Int32
                                  -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                            , WithStatus 404 RawHtml
                                                            , WithStatus 303 (Headers '[ Header "Location" RedirectUrl] NoContent ) ] )
        healthRecordDeleteHandler Nothing _ = respond =<< liftIO authFailToSignInView
        healthRecordDeleteHandler (Just account) recordId = do
          pool <- asks getPool
          record <- liftIO $ selectHealthRecord pool recordId account
          let authorizedRecord = record >>= checkRecordPermission account
          case authorizedRecord of
            Left err ->
              case err of
                ServerError 404 _ body _ -> do
                  html <- TP.htmlHandler context "/empty.html"
                  respond $ WithStatus @404 html
                  where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 body] )]
                ServerError 403 _ body _ -> do
                  html <- TP.htmlHandler context "/empty.html"
                  respond $ WithStatus @403 html
                  where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 body] )]
                _ -> throw err
            Right record -> liftIO $ do
              withResource pool $ \conn -> runBeamPostgres conn $
                runDelete $ delete (_healthHealthRecord healthDb)
                                   (\r -> _healthRecordId r ==. val_ (_healthRecordId record))
              red <- redirect ("/health_record" :: RedirectUrl)
              respond $ WithStatus @303 red

        healthRecordGetEditFormHandler ::Maybe SignInAccount
                                  -> Int32
                                  -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                            , WithStatus 404 RawHtml
                                                            , WithStatus 200 RawHtml ] )
        healthRecordGetEditFormHandler Nothing _ = respond =<< liftIO authFailToSignInView
        healthRecordGetEditFormHandler (Just account) recordId = do
          pool <- asks getPool
          dbResponse <- liftIO $ selectHealthRecord pool recordId account
          let authorizedRecord = dbResponse >>= checkRecordPermission account
          case authorizedRecord of
            Left (ServerError 404 _ body _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @404 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 body] )]

            Left (ServerError 403 _ body _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @403 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 body] )]

            Right record -> do
              let context = HS.fromList [ ( "healthRecordId", toJSON $ _healthRecordId record)
                                        , ( "healthRecord", toJSON PostHealthRecordData {
                                                                      postHeight = _healthRecordHeight record
                                                                    , postWeight = _healthRecordWeight record
                                                                    , postBodyFatPercentage = _healthRecordBodyFatPercentage record
                                                                    , postWaistlineCm = _healthRecordWaistlineCm record
                                                                    , postDate = _healthRecordDate record
                                                                   } )
                                        ]
              html <- TP.htmlHandler context "/health_record_form_edit.html"
              respond $ WithStatus @200 html

            _ -> throw err500

        healthRecordPutHandler :: Maybe SignInAccount
                               -> Int32
                               -> PostHealthRecordData
                               -> ReaderHandler ( Union '[ WithStatus 403 RawHtml
                                                         , WithStatus 404 RawHtml
                                                         , WithStatus 303 (Headers '[ Header "Location" RedirectUrl] NoContent ) ] )
        healthRecordPutHandler Nothing _ _ = respond =<< liftIO authFailToSignInView
        healthRecordPutHandler (Just account) recordId putHealthRecordData = do
          pool <- asks getPool
          record <- liftIO $ selectHealthRecord pool recordId account
          let authorizedRecord = record >>= checkRecordPermission account
          case authorizedRecord of
            Left (ServerError 404 _ body _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @404 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 body] )]

            Left (ServerError 403 _ body _) -> do
              html <- TP.htmlHandler context "/empty.html"
              respond $ WithStatus @403 html
              where context = HS.fromList [( "globalMsgs", toJSON [TLE.decodeUtf8 body] )]

            Right healthRecord -> do
              pool <- asks getPool
              liftIO $ updateHealthRecord pool healthRecord putHealthRecordData
              red <- redirect ("/health_record" :: RedirectUrl)
              respond $ WithStatus @303 red
              where
                    updateHealthRecord pool healthRecord putHealthRecordData =
                      withResource pool $ \conn -> runBeamPostgres conn $ do
                        runUpdate $ save (_healthHealthRecord healthDb) ( healthRecord {
                            _healthRecordHeight = postHeight putHealthRecordData
                          , _healthRecordWeight = postWeight putHealthRecordData
                          , _healthRecordBodyFatPercentage = postBodyFatPercentage putHealthRecordData
                          , _healthRecordWaistlineCm = postWaistlineCm putHealthRecordData
                          , _healthRecordDate = postDate putHealthRecordData
                        } )

            _ -> throw err500

recordErr404 :: ServerError
recordErr404 = err404 { errBody = TLE.encodeUtf8 "找不到這筆記錄" }
recordErr403 :: ServerError
recordErr403 = err403 { errBody = TLE.encodeUtf8 "你沒有權限修改這筆記錄" }


selectProfile :: Pool Connection -> SignInAccount -> IO (Either ServerError Profile)
selectProfile pool signInAccount = do
  profile <- withResource pool $ \conn -> runBeamPostgres conn $
    runSelectReturningOne $ select $
      filter_ (((AccountId . val_ . accountId) signInAccount ==.) . _profileAccountId) $
      all_ $ _healthProfile healthDb
  pure $ maybeToRight err403 { errBody = TLE.encodeUtf8 "你必須先建立個人檔案"} profile

selectHealthRecord :: Pool Connection
                    -> Int32
                    -> SignInAccount
                    -> IO (Either ServerError (Account, Profile, HealthRecord))
selectHealthRecord pool recordId signInAccount =
  withResource pool $ \conn -> runBeamPostgres conn $ do
    accountAndRecord <- runSelectReturningOne $ select $ do
      account <- all_ $ _healthAccount healthDb
      profile <- join_ (_healthProfile healthDb) $ (primaryKey account ==.) . _profileAccountId
      healthRecord <- join_ (_healthHealthRecord healthDb) $ (primaryKey profile ==.) . _healthRecordProfileId
      guard_ $ _healthRecordId healthRecord ==. val_ recordId
      pure (account, profile, healthRecord)
    pure $ maybeToRight recordErr404 accountAndRecord

checkRecordPermission :: SignInAccount
                      -> (Account, Profile, HealthRecord)
                      -> Either ServerError HealthRecord
checkRecordPermission signInAccount (account, _, record)
  | _accountId account == accountId signInAccount = Right record
  | otherwise                                     = Left recordErr403


