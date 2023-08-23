module Lib (main, start, app) where

import Lib.Auth.Jwt (generateJwk, sign, verify)
import Lib.Platform.Config (Config (Config), ServerConfig (ServerConfig), readConfig)
import Lib.Platform.Db (getConnection)
import Lib.Routes (routes)
import Network.Wai (Application)
import Web.Scotty

main :: IO ()
main = do
  config <- readConfig
  start config

start :: Config -> IO ()
start (Config db (ServerConfig port) jwtConfig) = do
  r <- getRoutes (Config db (ServerConfig port) jwtConfig)
  scotty port r

app :: IO Application
app = do
  config <- readConfig
  r <- getRoutes config
  scottyApp r

getRoutes :: Config -> IO (ScottyM ())
getRoutes (Config db _ jwtConfig) = do
  conn <- getConnection db
  jwk <- generateJwk jwtConfig
  let signToken = sign jwk jwtConfig
  let verifyToken = verify jwk
  return $ routes conn signToken verifyToken
