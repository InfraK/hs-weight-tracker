module Lib.Platform.Config
  ( Config (..),
    DBConfig (..),
    ServerConfig (..),
    JwtConfig (..),
    readConfig,
    readDBConfig,
    readJwtConfig,
    readServerConfig,
  )
where

import System.Posix.Env (getEnvDefault)

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

readConfig :: IO Config
readConfig = do
  db <- readDBConfig
  sv <- readServerConfig
  jwt <- readJwtConfig
  return $ Config db sv jwt

readDBConfig :: IO DBConfig
readDBConfig = do
  dbHost <- getEnvDefault "DB_HOST" "127.0.0.1"
  dbDatabase <- getEnvDefault "DB_DATABASE" "weight"
  dbUser <- getEnvDefault "DB_USER" "weight"
  dbPassword <- getEnvDefault "DB_PASSWORD" "weight"
  return $ DBConfig dbHost dbDatabase dbUser dbPassword

readServerConfig :: IO ServerConfig
readServerConfig = do
  portString <- getEnvDefault "PORT" "3000"
  return $ ServerConfig (read portString)

readJwtConfig :: IO JwtConfig
readJwtConfig = do
  expiry <- read <$> getEnvDefault "JWT_EXPIRY_MINUTES" "60"
  key <- getEnvDefault "JWT_KEY" "DEVELOPMENT_KEY"
  return $ JwtConfig expiry key
