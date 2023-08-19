module Lib (start) where

import Lib.Config (Config (Config), ServerConfig (ServerConfig), JwtConfig (JwtConfig))
import Lib.Db (getConnection)
import Lib.Routes (routes)
import Web.Scotty
import Lib.Auth.Jwt (generateJwk)

start :: Config -> IO ()
start (Config db (ServerConfig port)) = do
  conn <- getConnection db
  -- TODO pick up JWTConfig from env
  let config = JwtConfig 60 "myawesomekey"
  jwk <- generateJwk config
  scotty port $ routes conn jwk