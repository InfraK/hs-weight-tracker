module Lib.Platform.Crypto where

import Data.Password.Argon2 (Argon2, PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash, unPasswordHash), checkPassword, hashPassword, mkPassword)
import qualified Data.Text as T
import Lib.User (User, userPassword)
import Control.Monad.Cont (MonadIO)

verify :: (Monad m) => User -> T.Text -> m Bool
verify user plainPass = do
  let passwordCheck = checkPassword (mkPassword plainPass) (PasswordHash {unPasswordHash = userPassword user})
  return $ case passwordCheck of
    PasswordCheckFail -> False
    PasswordCheckSuccess -> True

hash :: (MonadIO m) => T.Text -> m (PasswordHash Argon2)
hash plain = hashPassword $ mkPassword plain