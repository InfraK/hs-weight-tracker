{-# LANGUAGE DeriveGeneric #-}

module Lib.Auth.Http where

import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Jose.Jwt (Jwt)
import Lib.Auth.Jwt (CurrentUser, Token, TokenError, UserId)
import Lib.Platform.Except (Except (UnAuthorized))
import Web.Scotty.Trans (ActionT, header, raise)

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

reqUser :: (Token -> IO (Either TokenError CurrentUser)) -> ActionT Except IO CurrentUser
reqUser verify = do
  maybeToken <- getAuthToken
  token <- case maybeToken of
    Nothing -> raise $ UnAuthorized "Bearer token not found"
    Just t -> return t
  eitherUserError <- liftIO $ verify token
  case eitherUserError of
    Left e -> raise $ UnAuthorized $ show e
    Right s -> return s

getAuthToken :: ActionT Except IO (Maybe Token)
getAuthToken = do
  maybeHeader <- header "Authorization"
  return $ parseHeader <$> maybeHeader
  where
    parseHeader txt = TL.toStrict $ last $ TL.words txt
