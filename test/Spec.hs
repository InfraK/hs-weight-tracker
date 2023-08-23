{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Database.PostgreSQL.Simple
import Lib (app)
import Lib.Platform.Config (readDBConfig)
import Lib.Platform.Db (getConnection)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

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

flushDb :: IO ()
flushDb = do
  dbConfig <- readDBConfig
  conn <- getConnection dbConfig
  _ <- execute_ conn "DELETE FROM users where email = 'my-email@email.com'"
  return ()
