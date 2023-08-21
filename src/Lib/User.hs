module Lib.User (User (..), CreateUser (..), findUser, findByEmail, createUser) where

import Data.Aeson
import Data.Password.Argon2 (Argon2, PasswordHash (unPasswordHash))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), query)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)

data User = User
  { userId :: Int,
    userEmail :: Text,
    userPassword :: Text,
    userCreatedAt :: UTCTime
  }

instance ToJSON User where
  toJSON (User uid email _ createdAt) =
    object
      [ "id" .= uid,
        "email" .= email,
        "createdAt" .= createdAt
      ]

type Email = Text

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

data CreateUser = CreateUser
  { createUserEmail :: Text,
    createUserPassword :: Text
  }

instance FromJSON CreateUser where
  parseJSON (Object v) = CreateUser <$> v .: "email" <*> v .: "password"
  parseJSON _ = fail "invalid input"

createUser :: Connection -> Email -> PasswordHash Argon2 -> IO [User]
createUser conn email hash =
  query
    conn
    "INSERT INTO users (email, password) VALUES (?, ?) RETURNING *"
    (email, unPasswordHash hash)

findUser :: Connection -> Int -> IO [User]
findUser conn uid =
  query
    conn
    "SELECT * FROM users where id = ?"
    $ Only uid

findByEmail :: Connection -> Email -> IO [User]
findByEmail conn email =
  query
    conn
    "SELECT * FROM users where email = ?"
    $ Only email
