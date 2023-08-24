module Lib.Routes (routes) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.PostgreSQL.Simple (Connection)
import Jose.Jwt (Jwt)
import Lib.Auth.Http (AuthForm (AuthForm), TokenPayload (TokenPayload), reqUser, returnForbidden)
import Lib.Auth.Jwt (CurrentUser, Token, TokenError, UserId)
import qualified Lib.Platform.Crypto as Crypto
import Lib.User (CreateUser (CreateUser), User (userId), createUser, findByEmail, findUser)
import Lib.Weight (CreateWeight (CreateWeight), createWeight, findWeights)
import Network.HTTP.Types (status401)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty
  ( ScottyM,
    finish,
    get,
    json,
    jsonData,
    middleware,
    param,
    post,
    status,
  )

routes :: Connection -> (UserId -> IO Jwt) -> (Token -> IO (Either TokenError CurrentUser)) -> ScottyM ()
routes conn signToken verifyToken = do
  middleware logStdout

  -- Users
  post "/login" $ do
    (AuthForm email pass) <- jsonData
    [user] <- liftIO $ Lib.User.findByEmail conn email
    isGood <- Crypto.verify user pass
    unless isGood (status status401 >> finish)

    token <- liftIO $ signToken $ userId user
    json $ TokenPayload token

  post "/signup" $ do
    (Lib.User.CreateUser email pass) <- jsonData
    hash <- Crypto.hash pass

    [user] <- liftIO $ Lib.User.createUser conn email hash
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
