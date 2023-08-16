{-# LANGUAGE OverloadedStrings #-}

module Lib.Routes (routes) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Simple (Connection)
import Lib.User (CreateUser (CreateUser), createUser, findUser, findUsers)
import Lib.Weight (CreateWeight (CreateWeight), createWeight, findWeights)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty
  ( ActionM,
    ScottyM,
    get,
    header,
    json,
    jsonData,
    middleware,
    param,
    post,
  )

routes :: Connection -> ScottyM ()
routes conn = do
  middleware logStdout

  -- Users
  get "/users" $ do
    users <- liftIO $ findUsers conn
    json users

  get "/users/:uid" $ do
    authHeader <- header "Authorization"
    liftIO $ print authHeader
    authToken <- getAuthToken
    liftIO $ print authToken

    uid <- param "uid"
    [user] <- liftIO $ findUser conn uid
    json user

  post "/users" $ do
    (CreateUser email) <- jsonData
    [user] <- liftIO $ createUser conn (CreateUser email)
    json user

  -- Weights
  get "/users/:uid/weights" $ do
    weights <- liftIO $ findWeights conn
    json weights

  post "/users/:uid/weights" $ do
    (CreateWeight grams) <- jsonData
    [weight] <- liftIO $ createWeight conn (CreateWeight grams)
    json weight

data AuthError
  = TokenNotFound
  deriving (Eq, Show)

type Token = T.Text

getAuthToken :: ActionM (Either AuthError Token)
getAuthToken = do
  maybeHeader <- header "Authorization"
  case maybeHeader of
    Just str -> do
      let token = TL.toStrict $ last $ TL.words str
      return $ Right token
    Nothing -> return $ Left TokenNotFound
