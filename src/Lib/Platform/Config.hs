module Lib.Platform.Config (Config (..), DBConfig (..), ServerConfig (..), JwtConfig (..)) where

data Config = Config
  { dbConfig :: DBConfig,
    svConfig :: ServerConfig,
    jwtConfig :: JwtConfig
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

data JwtConfig = JwtConfig
  { jwtConfigExpMinutes :: Int,
    jwtConfigKey :: String
  }