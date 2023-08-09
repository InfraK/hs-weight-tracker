{-# LANGUAGE OverloadedStrings #-}

module Weight (Weight (..), CreateWeight (..), createWeight, findWeights) where

import Data.Aeson
import Database.PostgreSQL.Simple (Connection, Only (Only), query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)

data Weight = Weight
  { weightId :: Int,
    weightWeightGrams :: Int
  }

instance ToJSON Weight where
  toJSON (Weight wid grams) =
    object
      [ "id" .= wid,
        "weightGrams" .= grams
      ]

instance FromRow Weight where
  fromRow = Weight <$> field <*> field

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
    "SELECT id, weight_grams FROM weights"