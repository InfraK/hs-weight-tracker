{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  middleware logStdout
  get "/:name" $ do
    name <- param "name"
    text $ "Hello " <> name
