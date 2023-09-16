module Lib (main, start, app) where

import Lib.Auth.Jwt (generateJwk, sign, verify)
import Lib.Platform.Config (Config (Config), ServerConfig (ServerConfig), readConfig)
import Lib.Platform.Db (createPool)
import Lib.Platform.Except (Except)
import Lib.Routes (routes)
import Network.Wai (Application)
import Web.Scotty.Trans (ScottyT, scottyT, scottyAppT)

main :: IO ()
main = do
  config <- readConfig
  start config

start :: Config -> IO ()
start (Config db (ServerConfig port) jwtConfig) = do
  r <- getRoutes (Config db (ServerConfig port) jwtConfig)
  scottyT port id r

app :: IO Application
app = do
  config <- readConfig
  r <- getRoutes config
  scottyAppT id r

getRoutes :: Config -> IO (ScottyT Except IO ())
getRoutes (Config db _ jwtConfig) = do
  connPool <- createPool db
  jwk <- generateJwk jwtConfig
  let signToken = sign jwk jwtConfig
  let verifyToken = verify jwk
  return $ routes connPool signToken verifyToken
