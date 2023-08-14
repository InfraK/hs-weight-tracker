module Lib.Db (getConnection) where

import Database.PostgreSQL.Simple
import Lib.Config (DBConfig (DBConfig))

getConnection :: DBConfig -> IO Connection
getConnection (DBConfig host db user pwd) =
  connect $
    defaultConnectInfo
      { connectHost = host,
        connectDatabase = db,
        connectUser = user,
        connectPassword = pwd
      }