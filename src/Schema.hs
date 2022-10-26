{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Database.Beam
import Data.Text.Lazy as TL
import Data.Int
import Data.Time

data AccountT f = Account { _accountId       :: Columnar f Int32
                          , _accountName     :: Columnar f TL.Text
                          , _accountPassword :: Columnar f TL.Text
                          } deriving (Generic, Beamable)

instance Table AccountT where
  data PrimaryKey AccountT f = AccountId (Columnar f Int32)
      deriving (Generic, Beamable)

  primaryKey = AccountId . _accountId


data SessionT f = Session { _sessionId        :: Columnar f TL.Text
                          , _sessionAccountId :: PrimaryKey AccountT f
                          , _sessionExpireAt  :: Columnar f LocalTime
                          } deriving (Generic, Beamable)

instance Table SessionT where
  data PrimaryKey SessionT f = SessionId (Columnar f TL.Text)
      deriving (Generic, Beamable)

  primaryKey = SessionId . _sessionId



data HealthDb f = HealthDb { _healthAccount :: f (TableEntity AccountT)
                           , _healthSession :: f (TableEntity SessionT)
                           } deriving (Generic, Database be)

healthDb :: DatabaseSettings be HealthDb
healthDb = defaultDbSettings `withDbModification`
  dbModification {
    _healthSession =
      modifyTableFields
        tableModification {
          -- session
          _sessionAccountId = AccountId (fieldNamed "account_id")
        }
  }
