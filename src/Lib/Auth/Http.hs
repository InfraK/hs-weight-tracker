module Lib.Auth.Http where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Jose.Jwt (Jwt (unJwt))
import Lib.Auth.Jwt (generateJwk, sign, verify)
import Lib.Config (JwtConfig (JwtConfig))

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