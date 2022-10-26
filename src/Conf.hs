module Conf where
import           Data.ByteString.Internal
import           Data.Pool
import           Database.Beam.Postgres

dbPool :: String -> IO (Pool Connection)
dbPool postgresUrl =
  newPool PoolConfig { createResource   = connectPostgreSQL $ packChars postgresUrl
                     , freeResource     = close
                     , poolCacheTTL     = 1
                     , poolMaxResources = 10
                     }

newtype Env = Env { getPool :: Pool Connection
                  }
