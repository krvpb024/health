{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

import Database.Beam
import Data.Text (Text)
import Data.Int (Int32)
import Data.ByteString.UTF8 as BSU
import Database.Beam.Backend
import Data.Time

data AccountT f = Account { _accountId       :: Columnar f (SqlSerial Int32)
                          , _accountName     :: Columnar f Text
                          , _accountPassword :: Columnar f Text
                          } deriving (Generic, Beamable)

instance Table AccountT where
  data PrimaryKey AccountT f = AccountId (Columnar f (SqlSerial Int32))
      deriving (Generic, Beamable)

  primaryKey = AccountId . _accountId


data SessionT f = Session { _sessionId       :: Columnar f (SqlSerial Int32)
                          , _sessionExpireAt :: Columnar f LocalTime
                          } deriving (Generic, Beamable)

instance Table SessionT where
  data PrimaryKey SessionT f = SessionId (Columnar f (SqlSerial Int32))
      deriving (Generic, Beamable)

  primaryKey = SessionId . _sessionId



data HealthDb f = HealthDb { _healthAccount :: f (TableEntity AccountT)
                           , _healthSession :: f (TableEntity SessionT)
                           } deriving (Generic, Database be)

healthDb :: DatabaseSettings be HealthDb
healthDb = defaultDbSettings
