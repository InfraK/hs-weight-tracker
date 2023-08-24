module Lib.Weight (Weight (..), CreateWeight (..), createWeight, findWeights) where

import Data.Aeson
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, Only (Only), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Lib.Auth.Jwt (UserId)

data Weight = Weight
  { weightId :: Int,
    weightUserId :: Int,
    weightWeightGrams :: Int,
    weightCreatedAt :: UTCTime
  }

instance ToJSON Weight where
  toJSON (Weight wid uid grams createdAt) =
    object
      [ "id" .= wid,
        "userId" .= uid,
        "weightGrams" .= grams,
        "createdAt" .= createdAt
      ]

instance FromRow Weight where
  fromRow = Weight <$> field <*> field <*> field <*> field

data CreateWeight = CreateWeight
  { createWeightGrams :: Int
  }

instance FromJSON CreateWeight where
  parseJSON (Object v) = CreateWeight <$> v .: "weightGrams"
  parseJSON _ = fail "invalid input"

createWeight :: Connection -> CreateWeight -> UserId -> IO Weight
createWeight conn (CreateWeight grams) uid = do
  [weight] <-
    query
      conn
      "INSERT INTO weights (weight_grams, user_id) VALUES (?, ?) RETURNING *"
      (grams, uid)
  return weight

findWeights :: Connection -> UserId -> IO [Weight]
findWeights conn uid =
  query
    conn
    "SELECT * FROM weights where user_id = ?"
    $ Only uid