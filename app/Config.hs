module Config (readDBConfig, readServerConfig) where

import Lib.Config (DBConfig (DBConfig), ServerConfig (ServerConfig))
import System.Posix.Env (getEnvDefault)

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