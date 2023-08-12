module Main where

import Config
import Lib (start)
import Lib.Config (Config (Config))

main :: IO ()
main = do
  db <- readDBConfig
  sv <- readServerConfig
  start $ Config db sv
