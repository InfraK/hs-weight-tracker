module Lib.Platform.Db (getConnection, singleResult) where

import Database.PostgreSQL.Simple
import Lib.Platform.Config (DBConfig (DBConfig))

getConnection :: DBConfig -> IO Connection
getConnection (DBConfig host db user pwd) =
  connect $
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
