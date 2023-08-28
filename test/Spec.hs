{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BLS
import Data.Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Database.PostgreSQL.Simple
import GHC.Generics
import Lib (app)
import Lib.Platform.Config (readDBConfig)
import Lib.Platform.Db (getConnection)
import Network.HTTP.Types (Method)
import Network.Wai.Test (SResponse (simpleBody))
import Test.Hspec (Spec, describe, hspec, it)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

data LoginPayload = LoginPayload
  { token :: Text,
    userId :: Int
  }
  deriving (Generic, Show)

instance Aeson.FromJSON LoginPayload

type Token = Text

type UserId = BS.ByteString

main :: IO ()
main = do
  flushDb
  hspec spec
  flushDb

spec :: Spec
spec = with app $ do
  describe "POST /signup" $ do
    it "responds with 200" $ do
      let postBody = [json|{email: "my-email@email.com", password: "my-password"}|]
      post "/signup" postBody
        `shouldRespondWith` 200

  describe "POST /login" $ do
    it "responds with 200 for valid credentials" $ do
      let postBody = [json|{email: "my-email@email.com", password: "my-password"}|]
      post "/login" postBody
        `shouldRespondWith` 200

    it "responds with 401 for invalid credentials" $ do
      let postBody = [json|{email: "my-email@email.com", password: "wrong-password"}|]
      post "/login" postBody
        `shouldRespondWith` 401

  describe "GET /users/:id" $ do
    it "responds with 401 for unauthenticated users" $ do
      get "/users/1" `shouldRespondWith` 401

    it "responds with 403 for other users" $ do
      (tkn, uid) <- login
      let differentId = uid <> "1"
      authRequest tkn "GET" ("/users/" <> differentId) "" `shouldRespondWith` 403

    it "responds with 200 to authenticated users" $ do
      (tkn, uid) <- login
      authRequest tkn "GET" ("/users/" <> uid) "" `shouldRespondWith` partialMatcher ("\"id\":" <> uid)

  describe "GET /users/:id/weights" $ do
    it "responds with 401 for unauthenticated users" $ do
      get "/users/1/weights" `shouldRespondWith` 401

    it "responds with 403 for other users" $ do
      (tkn, uid) <- login
      let differentId = uid <> "1"
      authRequest tkn "GET" ("/users/" <> differentId <> "/weights") "" `shouldRespondWith` 403

    it "responds with 200 to authenticated users" $ do
      (tkn, uid) <- login
      authRequest tkn "GET" ("/users/" <> uid <> "/weights") "" `shouldRespondWith` 200

  describe "POST /users/:id/weights" $ do
    it "responds with 401 for unauthenticated users" $ do
      post "/users/1/weights" "" `shouldRespondWith` 401

    it "responds with 403 for other users" $ do
      (tkn, uid) <- login
      let differentId = uid <> "1"
      authRequest tkn "POST" ("/users/" <> differentId <> "/weights") "" `shouldRespondWith` 403

    it "responds with 200 to authenticated users" $ do
      (tkn, uid) <- login
      let postBody = [json|{weightGrams: 3000}|]
      authRequest tkn "POST" ("/users/" <> uid <> "/weights") postBody `shouldRespondWith` partialMatcher "\"weightGrams\":3000"

authRequest :: Token -> Method -> BS.ByteString -> BLS.ByteString -> WaiSession st SResponse
authRequest tkn method path body = do
  let headers = [("Authorization", encodeUtf8 $ "Bearer " <> tkn), ("Content-Type", "application/json")]
  request method path headers body

login :: WaiSession st (Token, UserId)
login = do
  let postBody = [json|{email: "my-email@email.com", password: "my-password"}|]
  res <- post "/login" postBody
  let maybeLoginPayload = getLoginPayload res
  return $ case maybeLoginPayload of
    Just (LoginPayload tkn uid) -> (tkn, encodeUtf8 $ pack $ show uid)
    Nothing -> error "Could not login !!"

getLoginPayload :: SResponse -> Maybe LoginPayload
getLoginPayload res = Aeson.decode $ simpleBody res :: Maybe LoginPayload

flushDb :: IO ()
flushDb = do
  dbConfig <- readDBConfig
  conn <- getConnection dbConfig
  _ <- execute_ conn "DELETE FROM weights USING users WHERE weights.user_id = users.id AND users.email = 'my-email@email.com'"
  _ <- execute_ conn "DELETE FROM users where email = 'my-email@email.com'"
  return ()

partialMatcher :: BS.ByteString -> ResponseMatcher
partialMatcher expected =
  ResponseMatcher
    { matchStatus = 200,
      matchHeaders = [],
      matchBody =
        MatchBody
          ( \_ actual -> do
              if decodeUtf8 expected `isInfixOf` (decodeUtf8 . BS.concat . BLS.toChunks) actual
                then Nothing
                else Just ("Expected " <> (unpack . decodeUtf8 $ expected) <> "but found" <> (TL.unpack . TL.decodeUtf8) actual)
          )
    }