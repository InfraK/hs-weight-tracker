module Lib (start) where

import Lib.Auth.Jwt (generateJwk, sign, verify)
import Lib.Platform.Config (Config (Config), JwtConfig (JwtConfig), ServerConfig (ServerConfig))
import Lib.Platform.Db (getConnection)
import Lib.Routes (routes)
import Web.Scotty

start :: Config -> IO ()
start (Config db (ServerConfig port)) = do
  conn <- getConnection db
  -- TODO pick up JWTConfig from env
  let config = JwtConfig 60 "myawesomekey"
  jwk <- generateJwk config
  let signToken = sign jwk config
  let verifyToken = verify jwk
  scotty port $ routes conn signToken verifyToken