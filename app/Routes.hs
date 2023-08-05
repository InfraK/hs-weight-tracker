{-# LANGUAGE OverloadedStrings #-}
module Routes (routes) where

import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty
import User (users)

routes :: ScottyM ()
routes = do
  middleware logStdout
  get "/users" $ do
    json $ users