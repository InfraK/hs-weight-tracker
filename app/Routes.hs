{-# LANGUAGE OverloadedStrings #-}
module Routes (routes) where

import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty

routes :: ScottyM ()
routes = do
  middleware logStdout
  get "/:name" $ do
    name <- param "name"
    text $ "Hello " <> name