module Conf where
import Data.Pool
import Database.Beam.Postgres
import Data.ByteString.Internal

dbPool :: String -> IO (Pool Connection)
dbPool postgresUrl =
  newPool PoolConfig { createResource   = connectPostgreSQL $ packChars postgresUrl
                     , freeResource     = close
                     , poolCacheTTL     = 1
                     , poolMaxResources = 10
                     }

newtype Env = Env { pool :: Pool Connection
                  }
