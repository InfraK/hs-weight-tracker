{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Db (getConnection)
import Network.Wai.Middleware.RequestLogger (logStdout)
import User (CreateUser (CreateUser), createUser, findUser, findUsers)
import Web.Scotty
  ( ScottyM,
    get,
    json,
    jsonData,
    middleware,
    param,
    post,
  )

routes :: ScottyM ()
routes = do
  middleware logStdout
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