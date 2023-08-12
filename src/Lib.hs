module Lib (start) where

import Routes (routes)
import Web.Scotty

port :: Int
port = 3000

start :: IO ()
start =
  scotty port routes