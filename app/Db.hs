{-# LANGUAGE OverloadedStrings #-}
module Db (getConnection) where

import Database.PostgreSQL.Simple

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "127.0.0.1"
        , connectDatabase = "weight"
        , connectUser = "weight"
        , connectPassword = "weight"
        }

getConnection :: IO Connection
getConnection = connect localPG