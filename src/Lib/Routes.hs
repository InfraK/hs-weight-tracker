{-# LANGUAGE OverloadedStrings #-}

module Lib.Routes (routes) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import Jose.Jwk (Jwk)
import Lib.Auth.Http (AuthForm (AuthForm), TokenPayload (TokenPayload), reqUser)
import Lib.Auth.Jwt (sign)
import Lib.Config (JwtConfig (JwtConfig))
import Lib.User (CreateUser (CreateUser), User (userEmail), createUser, findByEmail, findUser, findUsers)
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

routes :: Connection -> Jwk -> ScottyM ()
routes conn jwk = do
  middleware logStdout

  -- Users
  post "/login" $ do
    -- TODO Not yet using passwords at all
    (AuthForm email _) <- jsonData
    [user] <- liftIO $ Lib.User.findByEmail conn email
    -- TODO Refactor so that JwtConfig is only used once
    token <- liftIO $ sign jwk (JwtConfig 60 "") $ T.pack $ userEmail user
    json $ TokenPayload token

  get "/users" $ do
    users <- liftIO $ Lib.User.findUsers conn
    json users

  get "/users/:uid" $ do
    authUser <- reqUser
    liftIO $ print authUser
    uid <- param "uid"
    [user] <- liftIO $ Lib.User.findUser conn uid
    json user

  post "/users" $ do
    (Lib.User.CreateUser email) <- jsonData
    [user] <- liftIO $ Lib.User.createUser conn (Lib.User.CreateUser email)
    json user

  -- Weights
  get "/users/:uid/weights" $ do
    weights <- liftIO $ findWeights conn
    json weights

  post "/users/:uid/weights" $ do
    (CreateWeight grams) <- jsonData
    [weight] <- liftIO $ createWeight conn (CreateWeight grams)
    json weight
