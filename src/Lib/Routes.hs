{-# LANGUAGE OverloadedStrings #-}

module Lib.Routes (routes) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib.User (CreateUser (CreateUser), createUser, findUser, findUsers)
import Lib.Weight (CreateWeight (CreateWeight), createWeight, findWeights)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty
  ( ScottyM,
    get,
    json,
    jsonData,
    middleware,
    param,
    post,
  )
import Database.PostgreSQL.Simple (Connection)

routes :: Connection -> ScottyM ()
routes conn = do
  middleware logStdout
  -- Users
  get "/users" $ do
    users <- liftIO $ findUsers conn
    json $ users

  get "/users/:uid" $ do
    uid <- param "uid"
    [user] <- liftIO $ findUser conn uid
    json $ user

  post "/users" $ do
    (CreateUser email) <- jsonData
    [user] <- liftIO $ createUser conn (CreateUser email)
    json $ user

  -- Weights
  get "/users/:uid/weights" $ do
    weights <- liftIO $ findWeights conn
    json $ weights

  post "/users/:uid/weights" $ do
    (CreateWeight grams) <- jsonData
    [weight] <- liftIO $ createWeight conn (CreateWeight grams)
    json $ weight