module Config (readDBConfig, readServerConfig, readJwtConfig) where

import Lib.Platform.Config (DBConfig (DBConfig), ServerConfig (ServerConfig), JwtConfig (JwtConfig))
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

readJwtConfig :: IO JwtConfig
readJwtConfig = do
  expiry <- read <$> getEnvDefault "JWT_EXPIRY_MINUTES" "60"
  key <- getEnvDefault "JWT_KEY" "DEVELOPMENT_KEY"
  return $ JwtConfig expiry key
