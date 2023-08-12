module Lib (start) where

import Lib.Config (Config (Config), ServerConfig (ServerConfig))
import Lib.Db (getConnection)
import Lib.Routes (routes)
import Web.Scotty

start :: Config -> IO ()
start (Config db (ServerConfig port)) = do
  conn <- getConnection db
  scotty port $ routes conn