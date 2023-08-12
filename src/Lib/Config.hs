module Lib.Config (DBConfig (..)) where

data DBConfig = DBConfig
  { configDBHost :: String,
    configDBDatabase :: String,
    configDBUser :: String,
    configDBPassword :: String
  }