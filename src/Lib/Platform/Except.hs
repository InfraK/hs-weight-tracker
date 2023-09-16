module Lib.Platform.Except (exceptHandler, Except (..)) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON, Value, object, toJSON, (.=))
import Data.String (fromString)
import Network.HTTP.Types (status400, status401, status403, status404, status500)
import Web.Scotty.Trans (ActionT, ScottyError (showError, stringError), json, status)

data Except
  = BadRequest String
  | Forbidden
  | NotFound String
  | UnAuthorized String
  | StringEx String
  deriving (Show, Eq)

instance ToJSON Except where
  toJSON (BadRequest msg) = errorObject msg
  toJSON (NotFound msg) = errorObject msg
  toJSON Forbidden = errorObject "Forbidden"
  toJSON (UnAuthorized msg) = errorObject msg
  toJSON (StringEx msg) = errorObject msg

instance ScottyError Except where
  stringError = StringEx
  showError = fromString . show

errorObject :: String -> Value
errorObject msg = object ["error" .= msg]

exceptHandler :: MonadIO m => Except -> ActionT Except m ()
exceptHandler (BadRequest s) = do
  status status400
  json $ BadRequest s
exceptHandler (UnAuthorized s) = do
  status status401
  json $ UnAuthorized s
exceptHandler Forbidden = do
  status status403
  json Forbidden
exceptHandler (NotFound s) = do
  status status404
  json $ NotFound s
exceptHandler (StringEx s) = do
  liftIO $ print $ "Uncaught exception: " <> s
  status status500
  json $ StringEx "Internal server error."
