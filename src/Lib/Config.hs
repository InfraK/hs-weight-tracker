module Lib.Config (Config (..), DBConfig (..), ServerConfig (..)) where

data Config = Config
  { dbConfig :: DBConfig,
    svConfig :: ServerConfig
  }

data DBConfig = DBConfig
  { dbConfigDBHost :: String,
    dbConfigDBDatabase :: String,
    dbConfigDBUser :: String,
    dbConfigDBPassword :: String
  }

data ServerConfig = ServerConfig
  { serverConfigPort :: Int
  }