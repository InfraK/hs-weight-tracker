{-# LANGUAGE DeriveGeneric #-}

module Lib.Auth.Http where

import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Jose.Jwt (Jwt)
import Lib.Auth.Jwt (CurrentUser, Token, TokenError (TokenNotFound))
import Network.HTTP.Types.Status (status401, status403)
import Web.Scotty (ActionM, finish, header, json, status)
import Web.Scotty.Trans (ActionT)

data AuthForm = AuthForm
  { authEmail :: Text,
    authPassword :: Text
  }

instance FromJSON AuthForm where
  parseJSON (Object v) = AuthForm <$> v .: "email" <*> v .: "password"
  parseJSON _ = fail "invalid input"

data TokenPayload = TokenPayload
  { tokenPayloadToken :: Jose.Jwt.Jwt
  }

instance ToJSON TokenPayload where
  toJSON (TokenPayload token) =
    object
      ["token" .= token]

reqUser :: (Token -> IO (Either TokenError CurrentUser)) -> ActionM CurrentUser
reqUser verify = do
  user <- getAuthUser verify
  case user of
    Left e -> tokenErrorHandler e
    Right s -> return s
  where
    tokenErrorHandler e = do
      status status401 >> json e >> finish

getAuthUser :: (Token -> IO (Either TokenError CurrentUser)) -> ActionT TL.Text IO (Either TokenError CurrentUser)
getAuthUser verify = do
  maybeToken <- getAuthToken
  case maybeToken of
    Nothing -> return $ Left TokenNotFound
    Just token -> liftIO $ verify token

getAuthToken :: ActionT TL.Text IO (Maybe Token)
getAuthToken = do
  maybeHeader <- header "Authorization"
  return $ parseHeader <$> maybeHeader
  where
    parseHeader txt = TL.toStrict $ last $ TL.words txt

returnForbidden :: ActionM ()
returnForbidden = status status403 >> finish