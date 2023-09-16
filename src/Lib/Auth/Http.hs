{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Lib.Auth.Http where

import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Jose.Jwt (Jwt)
import Lib.Auth.Jwt (CurrentUser, Token, TokenError (TokenNotFound), UserId)
import Network.HTTP.Types.Status (status401, status403)
import Web.Scotty.Trans (ActionT, finish, header, json, status)

data AuthForm = AuthForm
  { authEmail :: Text,
    authPassword :: Text
  }

instance FromJSON AuthForm where
  parseJSON (Object v) = AuthForm <$> v .: "email" <*> v .: "password"
  parseJSON _ = fail "invalid input"

data LoginPayload = LoginPayload
  { loginPayloadToken :: Jose.Jwt.Jwt,
    loginPayloadUserId :: UserId
  }

instance ToJSON LoginPayload where
  toJSON (LoginPayload token uid) =
    object
      [ "token" .= token,
        "userId" .= uid
      ]

reqUser :: (Token -> IO (Either TokenError CurrentUser)) -> ActionT TL.Text IO CurrentUser
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

returnForbidden :: ActionT TL.Text IO ()
returnForbidden = status status403 >> finish