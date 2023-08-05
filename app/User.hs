{-# LANGUAGE OverloadedStrings #-}

module User (User (..), CreateUser (..), users, createUser) where

import Data.Aeson

data User = User
  { userId :: String,
    userEmail :: String
  }

instance ToJSON User where
  toJSON (User uid email) =
    object
      [ "id" .= uid,
        "email" .= email
      ]

data CreateUser = CreateUser
  { createUserEmail :: String
  }

instance FromJSON CreateUser where
  parseJSON (Object v) = CreateUser <$> v .: "email"
  parseJSON _ = fail "invalid input"

createUser :: CreateUser -> User
createUser (CreateUser email) = User {userId = "uid-11", userEmail = email}

users :: [User]
users = [santi]

santi :: User
santi = User {userId = "uid-1", userEmail = "santiagokent@gmail.com"}