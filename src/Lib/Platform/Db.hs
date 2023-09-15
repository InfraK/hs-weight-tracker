module Lib.Platform.Db (singleResult, createPool, openConn) where

import Data.Pool (Pool, defaultPoolConfig, newPool)
import Database.PostgreSQL.Simple
import Lib.Platform.Config (DBConfig (DBConfig))

createPool :: DBConfig -> IO (Pool Connection)
createPool config = newPool poolConfig
  where
    poolConfig = defaultPoolConfig (openConn config) close 1 10

openConn :: DBConfig -> IO Connection
openConn = connect . parseConfig

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
