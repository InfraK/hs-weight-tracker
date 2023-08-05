{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Network.Wai.Middleware.RequestLogger (logStdout)
import User (CreateUser (CreateUser), findUsers, createUser)
import Web.Scotty
import Db (getConnection)
import Control.Monad.IO.Class (MonadIO(liftIO))

routes :: ScottyM ()
routes = do
  middleware logStdout
  get "/users" $ do
    conn <- liftIO $ getConnection
    users <- liftIO $ findUsers conn
    json $ users
  post "/users" $ do
    (CreateUser email) <- jsonData
    json $ createUser (CreateUser email)