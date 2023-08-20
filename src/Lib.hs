module Lib (start) where

import Lib.Auth.Jwt (generateJwk, sign, verify)
import Lib.Platform.Config (Config (Config), ServerConfig (ServerConfig))
import Lib.Platform.Db (getConnection)
import Lib.Routes (routes)
import Web.Scotty

start :: Config -> IO ()
start (Config db (ServerConfig port) jwtConfig) = do
  conn <- getConnection db
  jwk <- generateJwk jwtConfig
  let signToken = sign jwk jwtConfig
  let verifyToken = verify jwk
  scotty port $ routes conn signToken verifyToken