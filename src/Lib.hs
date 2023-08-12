module Lib (start) where

import Lib.Routes (routes)
import Web.Scotty
import Lib.Config (DBConfig)
import Lib.Db (getConnection)

port :: Int
port = 3000

start :: DBConfig -> IO ()
start config = do
  conn <- getConnection config
  scotty port $ routes conn