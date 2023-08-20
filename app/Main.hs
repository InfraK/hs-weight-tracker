module Main where

import Config
import Lib (start)
import Lib.Platform.Config (Config (Config))

main :: IO ()
main = do
  db <- readDBConfig
  sv <- readServerConfig
  jwt <- readJwtConfig
  start $ Config db sv jwt
