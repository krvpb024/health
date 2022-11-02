{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Schema where

import Database.Beam
import Data.Text.Lazy as TL
import Data.Int
import Data.Time
import Data.Fixed
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Data.Scientific

data AccountT f = Account { _accountId       :: Columnar f Int32
                          , _accountName     :: Columnar f TL.Text
                          , _accountPassword :: Columnar f TL.Text
                          } deriving (Generic, Beamable)

type Account = AccountT Identity
type AccountId = PrimaryKey AccountT Identity

deriving instance Show Account
deriving instance Eq Account

deriving instance Show AccountId
deriving instance Eq AccountId

instance Table AccountT where
  data PrimaryKey AccountT f = AccountId (Columnar f Int32)
      deriving (Generic, Beamable)

  primaryKey :: AccountT column -> PrimaryKey AccountT column
  primaryKey = AccountId . _accountId

data SessionT f = Session { _sessionId        :: Columnar f TL.Text
                          , _sessionAccountId :: PrimaryKey AccountT f
                          , _sessionExpireAt  :: Columnar f LocalTime
                          } deriving (Generic, Beamable)

type Session = SessionT Identity
type SessionId = PrimaryKey SessionT Identity

deriving instance Show Session
deriving instance Eq Session

deriving instance Show SessionId
deriving instance Eq SessionId

instance Table SessionT where
  data PrimaryKey SessionT f = SessionId (Columnar f TL.Text)
      deriving (Generic, Beamable)

  primaryKey :: SessionT column -> PrimaryKey SessionT column
  primaryKey = SessionId . _sessionId

data SessionMessageT f = SessionMessage { _sessionMessageId        :: Columnar f Int32
                                        , _sessionMessageSessionId :: PrimaryKey SessionT f
                                        , _sessionMessageContent   :: Columnar f TL.Text
                                        } deriving (Generic, Beamable)

type SessionMessage = SessionMessageT Identity
type SessionMessageId = PrimaryKey SessionMessageT Identity

deriving instance Show SessionMessage
deriving instance Eq SessionMessage

instance Table SessionMessageT where
  data PrimaryKey SessionMessageT f = SessionMessageId (Columnar f Int32)
      deriving (Generic, Beamable)

  primaryKey :: SessionMessageT column -> PrimaryKey SessionMessageT column
  primaryKey = SessionMessageId . _sessionMessageId

data ProfileT f = Profile { _profileId         :: Columnar f Int32
                          , _profileAccountId  :: PrimaryKey AccountT f
                          , _profileGender     :: Columnar f Bool
                          , _profileBirthDate  :: Columnar f Day
                          , _profileInitHeight :: Columnar f Int32
                          } deriving (Generic, Beamable)

type Profile = ProfileT Identity
type ProfileId = PrimaryKey ProfileT Identity

deriving instance Show Profile
deriving instance Eq Profile

deriving instance Show ProfileId
deriving instance Eq ProfileId

instance Table ProfileT where
  data PrimaryKey ProfileT f = ProfileId (Columnar f Int32)
      deriving (Generic, Beamable)

  primaryKey :: ProfileT column -> PrimaryKey ProfileT column
  primaryKey = ProfileId . _profileId

data HealthRecordT f = HealthRecord { _healthRecordId                :: Columnar f Int32
                                    , _healthRecordProfileId         :: PrimaryKey ProfileT f
                                    , _healthRecordHeight            :: Columnar f Int32
                                    , _healthRecordWeight            :: Columnar f Scientific
                                    , _healthRecordBodyFatPercentage :: Columnar f (Maybe Scientific)
                                    , _healthRecordWaistlineCm       :: Columnar f (Maybe Scientific)
                                    , _healthRecordRecordAt          :: Columnar f LocalTime
                                    } deriving (Generic, Beamable)

type HealthRecord = HealthRecordT Identity
type HealthRecordId = PrimaryKey HealthRecordT Identity

deriving instance Show HealthRecord
deriving instance Eq HealthRecord

instance Table HealthRecordT where
  data PrimaryKey HealthRecordT f = HealthRecordId (Columnar f Int32)
      deriving (Generic, Beamable)

  primaryKey :: HealthRecordT column -> PrimaryKey HealthRecordT column
  primaryKey = HealthRecordId . _healthRecordId

data HealthDb f = HealthDb { _healthAccount        :: f (TableEntity AccountT)
                           , _healthSession        :: f (TableEntity SessionT)
                           , _healthSessionMessage :: f (TableEntity SessionMessageT)
                           , _healthProfile        :: f (TableEntity ProfileT)
                           , _healthHealthRecord   :: f (TableEntity HealthRecordT)
                           } deriving (Generic, Database be)

healthDb :: DatabaseSettings be HealthDb
healthDb = defaultDbSettings `withDbModification`
  dbModification { _healthSession =
                      modifyTableFields
                      tableModification {
                        _sessionAccountId = AccountId (fieldNamed "account_id")
                      }
                 , _healthSessionMessage =
                      modifyTableFields
                      tableModification {
                        _sessionMessageSessionId = SessionId (fieldNamed "session_id")
                      }
                 , _healthProfile =
                      modifyTableFields
                      tableModification {
                        _profileAccountId = AccountId (fieldNamed "account_id")
                      }
                , _healthHealthRecord =
                      modifyTableFields
                      tableModification {
                        _healthRecordId                = fieldNamed "id"
                      , _healthRecordProfileId         = ProfileId (fieldNamed "profile_id")
                      , _healthRecordHeight            = fieldNamed "height"
                      , _healthRecordWeight            = fieldNamed "weight"
                      , _healthRecordBodyFatPercentage = fieldNamed "body_fat_percentage"
                      , _healthRecordWaistlineCm       = fieldNamed "waistline_cm"
                      , _healthRecordRecordAt          = fieldNamed "record_at"
                      }
  }
