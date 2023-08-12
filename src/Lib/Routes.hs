{-# LANGUAGE OverloadedStrings #-}

module Lib.Routes (routes) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib.Db (getConnection)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Lib.User (CreateUser (CreateUser), createUser, findUser, findUsers)
import Web.Scotty
  ( ScottyM,
    get,
    json,
    jsonData,
    middleware,
    param,
    post,
  )
import Lib.Weight (CreateWeight (CreateWeight), createWeight, findWeights)

routes :: ScottyM ()
routes = do
  middleware logStdout

  -- Users
  get "/users" $ do
    conn <- liftIO $ getConnection
    users <- liftIO $ findUsers conn
    json $ users

  get "/users/:uid" $ do
    uid <- param "uid"
    conn <- liftIO $ getConnection
    [user] <- liftIO $ findUser conn uid
    json $ user

  post "/users" $ do
    (CreateUser email) <- jsonData
    conn <- liftIO $ getConnection
    [user] <- liftIO $ createUser conn (CreateUser email)
    json $ user

  -- Weights
  get "/users/:uid/weights" $ do
    conn <- liftIO $ getConnection
    weights <- liftIO $ findWeights conn
    json $ weights

  post "/users/:uid/weights" $ do
    (CreateWeight grams) <- jsonData
    conn <- liftIO $ getConnection
    [weight] <- liftIO $ createWeight conn (CreateWeight grams)
    json $ weight