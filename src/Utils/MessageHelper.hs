module Utils.MessageHelper where

-- import Data.Text.Lazy as TL
-- import Data.Pool
-- import Database.Beam.Postgres
-- import Database.Beam
-- import Schema

-- extractMessages :: Pool Connection -> TL.Text -> IO [TL.Text]
-- extractMessages pool sessionId = do
--   withResource pool $ \conn -> runBeamPostgres conn $
--     runSelectReturningList $ select $
--     filter_ (\session -> _sessionId session ==. val_ sessionId) $
--     all_ (_healthSession healthDb)

