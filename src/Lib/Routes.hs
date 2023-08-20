{-# LANGUAGE OverloadedStrings #-}

module Lib.Routes (routes) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.PostgreSQL.Simple (Connection)
import Jose.Jwt (Jwt)
import Lib.Auth.Http (AuthForm (AuthForm), TokenPayload (TokenPayload), reqUser, returnForbidden)
import Lib.Auth.Jwt (CurrentUser, Token, TokenError, UserId)
import Lib.User (CreateUser (CreateUser), User (userId), createUser, findByEmail, findUser)
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

routes :: Connection -> (UserId -> IO Jwt) -> (Token -> IO (Either TokenError CurrentUser)) -> ScottyM ()
routes conn signToken verifyToken = do
  middleware logStdout

  -- Users
  post "/login" $ do
    -- TODO Not yet using passwords at all
    (AuthForm email _) <- jsonData
    [user] <- liftIO $ Lib.User.findByEmail conn email
    token <- liftIO $ signToken $ userId user
    json $ TokenPayload token

  post "/signup" $ do
    (Lib.User.CreateUser email) <- jsonData
    [user] <- liftIO $ Lib.User.createUser conn (Lib.User.CreateUser email)
    json user

  get "/users/:uid" $ do
    (_, tokenUserId) <- reqUser verifyToken
    uid <- param "uid"
    when (tokenUserId /= uid) returnForbidden

    [user] <- liftIO $ Lib.User.findUser conn uid
    json user

  -- Weights
  get "/users/:uid/weights" $ do
    (_, tokenUserId) <- reqUser verifyToken
    uid <- param "uid"
    when (tokenUserId /= uid) returnForbidden

    weights <- liftIO $ findWeights conn
    json weights

  post "/users/:uid/weights" $ do
    (_, tokenUserId) <- reqUser verifyToken
    uid <- param "uid"
    when (tokenUserId /= uid) returnForbidden

    (CreateWeight grams) <- jsonData
    [weight] <- liftIO $ createWeight conn (CreateWeight grams)
    json weight
