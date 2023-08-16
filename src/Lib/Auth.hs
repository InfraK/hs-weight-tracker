module Lib.Auth (encodeDecodePrint, sign, verify, privateJwk) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Random (MonadRandom)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Jose.Jwa
import Jose.Jwk (Jwk, KeyUse (Sig), generateRsaKeyPair)
import Jose.Jwt
import Lib.Config (JwtConfig (JwtConfig))

data AuthData = AuthData
  { authDataSub :: String
  }
  deriving (Show)

data TokenError
  = TokenNotFound
  | TokenErrorMalformed String
  | TokenErrorExpired
  | TokenErrorUserIdNotFound
  deriving (Eq, Show)

sign :: Jwk -> JwtConfig -> T.Text -> IO Jwt
sign jwk (JwtConfig expMinutes _) sub = do
  claims <- makeJwtClaims sub (fromIntegral expMinutes)
  let payload = makePayload claims
  eitherJwt <- encode [jwk] encAlg payload
  case eitherJwt of
    Right jwt -> do
      return jwt
    _ -> error "Could not sign JWT, check config"

verify :: (MonadIO m, MonadRandom m) => Jwk -> T.Text -> m (Either TokenError AuthData)
verify jwk jwt = do
  eitherJwt <- decode [jwk] (Just encAlg) (T.encodeUtf8 jwt)
  curTime <- liftIO getPOSIXTime
  return $ do
    jwtContent <- first (TokenErrorMalformed . show) eitherJwt
    claimsRaw <- case jwtContent of
      Jws (_, claimsRaw) -> Right claimsRaw
      _ -> Left $ TokenErrorMalformed "Not Jws"
    jwtClaims <- first TokenErrorMalformed $ Aeson.eitherDecode $ BL.fromStrict claimsRaw
    let (IntDate expiredAt) = fromMaybe (IntDate curTime) $ jwtExp jwtClaims
    when (expiredAt < curTime) $ Left TokenErrorExpired
    uid <- case jwtSub jwtClaims of
      Just sub -> Right $ T.unpack sub
      Nothing -> Left TokenErrorUserIdNotFound
    return $ AuthData uid

privateJwk :: JwtConfig -> IO Jwk
privateJwk (JwtConfig _ key) = do
  (_, privKey) <- generateRsaKeyPair 256 (KeyId key) Sig (Just (Signed RS256))
  return privKey

makeJwtClaims :: T.Text -> NominalDiffTime -> IO JwtClaims
makeJwtClaims sub expMinutes = do
  currentUTC <- getCurrentTime
  let expiry = IntDate $ utcTimeToPOSIXSeconds $ addUTCTime (expMinutes * 60) currentUTC
  return $
    JwtClaims
      (Just "hs-weight-tracker")
      (Just sub)
      Nothing
      (Just expiry)
      Nothing
      Nothing
      Nothing

makePayload :: JwtClaims -> Payload
makePayload claims = Claims $ BL.toStrict $ Aeson.encode claims

encAlg :: JwtEncoding
encAlg = JwsEncoding RS256

encodeDecodePrint :: IO ()
encodeDecodePrint = do
  let config = JwtConfig 60 "myawesomekey"
  jwk <- privateJwk config
  jwt <- sign jwk config "uuid"
  print jwt
  print $ unJwt jwt
  content <- verify jwk (bsToText $ unJwt jwt)
  case content of
    Right (AuthData uid) -> do
      print uid
    (Left err) -> do
      print err

bsToText :: B.ByteString -> T.Text
bsToText = T.decodeUtf8