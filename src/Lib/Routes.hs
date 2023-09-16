module Lib.Routes (routes) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import Jose.Jwt (Jwt)
import Lib.Auth.Http (AuthForm (AuthForm), LoginPayload (LoginPayload), reqUser)
import Lib.Auth.Jwt (CurrentUser, Token, TokenError, UserId)
import qualified Lib.Platform.Crypto as Crypto
import Lib.Platform.Except (Except (BadRequest, Forbidden, NotFound, UnAuthorized), exceptHandler)
import Lib.User (CreateUser (CreateUser), User (userId), createUser, findByEmail, findUser)
import Lib.Weight (CreateWeight (CreateWeight), createWeight, findWeights)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans
  ( ScottyT,
    defaultHandler,
    get,
    json,
    jsonData,
    middleware,
    param,
    post,
    raise,
  )

routes :: Pool Connection -> (UserId -> IO Jwt) -> (Token -> IO (Either TokenError CurrentUser)) -> ScottyT Except IO ()
routes connPool signToken verifyToken = do
  middleware logStdout
  defaultHandler exceptHandler

  -- Users
  post "/login" $ do
    (AuthForm email pass) <- jsonData
    maybeUser <- liftIO $ withResource connPool (\conn -> Lib.User.findByEmail conn email)
    user <- case maybeUser of
      Nothing -> raise $ UnAuthorized "Invalid email or password"
      Just user -> return user

    isGood <- Crypto.verify user pass
    unless isGood (raise $ UnAuthorized "Invalid email or password")
    let uid = userId user
    token <- liftIO $ signToken uid
    json $ LoginPayload token uid

  post "/signup" $ do
    (Lib.User.CreateUser email pass) <- jsonData
    hash <- Crypto.hash pass
    userOrError <- liftIO $ withResource connPool (\conn -> Lib.User.createUser conn email hash)
    case userOrError of
      Left err -> raise $ BadRequest $ show err
      Right user -> json user

  get "/users/:uid" $ do
    (_, tokenUserId) <- reqUser verifyToken
    uid <- param "uid"
    when (tokenUserId /= uid) (raise Forbidden)

    maybeUser <- liftIO $ withResource connPool (\conn -> Lib.User.findUser conn uid)

    case maybeUser of
      Nothing -> raise $ NotFound $ "could not find user with id: " <> show uid
      Just user -> json user

  -- Weights
  get "/users/:uid/weights" $ do
    (_, tokenUserId) <- reqUser verifyToken
    uid <- param "uid"
    when (tokenUserId /= uid) (raise Forbidden)

    weights <- liftIO $ withResource connPool (\conn -> findWeights conn uid)
    json weights

  post "/users/:uid/weights" $ do
    (_, tokenUserId) <- reqUser verifyToken
    uid <- param "uid"
    when (tokenUserId /= uid) (raise Forbidden)

    (CreateWeight grams) <- jsonData
    weight <- liftIO $ withResource connPool (\conn -> createWeight conn (CreateWeight grams) uid)
    json weight
