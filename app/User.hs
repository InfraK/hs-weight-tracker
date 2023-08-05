{-# LANGUAGE OverloadedStrings #-}

module User (User (..), CreateUser (..), findUsers, createUser) where

import Data.Aeson
import Database.PostgreSQL.Simple (Connection, FromRow, query_)
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

createUser :: CreateUser -> User
createUser (CreateUser email) = User {userId = 1, userEmail = email}

findUsers :: Connection -> IO [User]
findUsers conn = query_ conn "SELECT * FROM users;"
