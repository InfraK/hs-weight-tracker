{-# LANGUAGE DeriveGeneric #-}

module Lib.Auth.Http where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Jose.Jwt (Jwt (unJwt))
import Lib.Auth.Jwt (CurrentUser, generateJwk, sign, verify)
import Lib.Config (JwtConfig (JwtConfig))
import Network.HTTP.Types.Status (status401)
import Web.Scotty (ActionM, finish, header, json, status)
import Web.Scotty.Trans (ActionT)

data AuthError
  = TokenNotFound
  deriving (Eq, Show, Generic)

type Token = T.Text

instance ToJSON AuthError

reqUser :: ActionM (CurrentUser)
reqUser = do
  user <- getAuthUser
  case user of
    Left e -> tokenErrorHandler e
    Right s -> return s
  where
    tokenErrorHandler e = do
      status status401 >> json e >> finish

getAuthUser :: ActionT TL.Text IO (Either AuthError CurrentUser)
getAuthUser = do
  maybeToken <- getAuthToken
  return $ case maybeToken of
    Nothing -> Left TokenNotFound
    -- TODO validate the token
    Just token -> Right (token, "uid")

getAuthToken :: ActionT TL.Text IO (Maybe Token)
getAuthToken = do
  maybeHeader <- header "Authorization"
  return $ parseHeader <$> maybeHeader
  where
    parseHeader txt = TL.toStrict $ last $ TL.words txt

data AuthForm = AuthForm
  { authEmail :: String,
    authPassword :: String
  }

instance FromJSON AuthForm where
  parseJSON (Object v) = AuthForm <$> v .: "email" <*> v .: "password"
  parseJSON _ = fail "invalid input"

data TokenPayload = TokenPayload
  { tokenPayloadToken :: Jwt
  }

instance ToJSON TokenPayload where
  toJSON (TokenPayload token) =
    object
      ["token" .= token]

-- TODO REMOVE when JWT Flow is complete
encodeDecodePrint :: IO ()
encodeDecodePrint = do
  let config = JwtConfig 60 "myawesomekey"
  jwk <- generateJwk config
  jwt <- sign jwk config "uuid"
  print jwt
  print $ unJwt jwt
  content <- verify jwk (bsToText $ unJwt jwt)
  case content of
    Right (_, uid) -> do
      print uid
    (Left err) -> do
      print err

bsToText :: B.ByteString -> T.Text
bsToText = T.decodeUtf8