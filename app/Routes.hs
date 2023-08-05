{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Network.Wai.Middleware.RequestLogger (logStdout)
import User (CreateUser (CreateUser), users, createUser)
import Web.Scotty

routes :: ScottyM ()
routes = do
  middleware logStdout
  get "/users" $ do
    json $ users
  post "/users" $ do
    (CreateUser email) <- jsonData
    json $ createUser (CreateUser email)