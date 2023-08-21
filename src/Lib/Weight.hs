module Lib.Weight (Weight (..), CreateWeight (..), createWeight, findWeights) where

import Data.Aeson
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, Only (Only), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)

data Weight = Weight
  { weightId :: Int,
    weightWeightGrams :: Int,
    weightCreatedAt :: UTCTime
  }

instance ToJSON Weight where
  toJSON (Weight wid grams createdAt) =
    object
      [ "id" .= wid,
        "weightGrams" .= grams,
        "createdAt" .= createdAt
      ]

instance FromRow Weight where
  fromRow = Weight <$> field <*> field <*> field

data CreateWeight = CreateWeight
  { createWeightGrams :: Int
  }

instance FromJSON CreateWeight where
  parseJSON (Object v) = CreateWeight <$> v .: "weightGrams"
  parseJSON _ = fail "invalid input"

createWeight :: Connection -> CreateWeight -> IO [Weight]
createWeight conn (CreateWeight grams) =
  query
    conn
    "INSERT INTO weights (weight_grams) VALUES (?) RETURNING *"
    $ Only grams

findWeights :: Connection -> IO [Weight]
findWeights conn =
  query_
    conn
    "SELECT * FROM weights"