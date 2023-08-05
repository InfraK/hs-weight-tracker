module Main where

import Routes (routes)
import Web.Scotty

port :: Int
port = 3000

main :: IO ()
main =
  scotty port routes
