module Lib.Platform.Db (singleResult, createPool) where

import Data.Pool (Pool, defaultPoolConfig, newPool)
import Database.PostgreSQL.Simple
import Lib.Platform.Config (DBConfig (DBConfig))

createPool :: DBConfig -> IO (Pool Connection)
createPool config = newPool poolConfig
  where
    poolConfig = defaultPoolConfig (connect $ parseConfig config) close 1 10

parseConfig :: DBConfig -> ConnectInfo
parseConfig (DBConfig host db user pwd) =
  defaultConnectInfo
    { connectHost = host,
      connectDatabase = db,
      connectUser = user,
      connectPassword = pwd
    }

singleResult :: [a] -> Maybe a
singleResult [x] = Just x
singleResult [] = Nothing
singleResult _ = Nothing
