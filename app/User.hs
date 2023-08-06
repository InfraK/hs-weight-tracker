{-# LANGUAGE OverloadedStrings #-}

module User (User (..), CreateUser (..), findUsers, findUser, createUser) where

import Data.Aeson
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)

data User = User
  { userId :: Int,
    userEmail :: String
  }

instance ToJSON User where
  toJSON (User uid email) =
    object
      [ "id" .= uid,
        "email" .= email
      ]

instance FromRow User where
  fromRow = User <$> field <*> field

data CreateUser = CreateUser
  { createUserEmail :: String
  }

instance FromJSON CreateUser where
  parseJSON (Object v) = CreateUser <$> v .: "email"
  parseJSON _ = fail "invalid input"

createUser :: Connection -> CreateUser -> IO [User]
createUser conn (CreateUser email) =
  query
    conn
    "INSERT INTO users (email) VALUES (?) RETURNING *"
    $ Only email

findUsers :: Connection -> IO [User]
findUsers conn =
  query_
    conn
    "SELECT * FROM users;"

findUser :: Connection -> Int -> IO [User]
findUser conn uid =
  query
    conn
    "SELECT * FROM users where id = ?"
    $ Only uid
