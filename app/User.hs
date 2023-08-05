{-# LANGUAGE OverloadedStrings #-}

module User (User (..), users) where

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

users :: [User]
users = [santi]

santi :: User
santi = User {userId = "uid-1", userEmail = "santiagokent@gmail.com"}