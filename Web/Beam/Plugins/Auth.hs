{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Web.Beam.Plugins.Auth 
  ( AuthUser (..)
  , AuthContainer (..)
  , AuthApp (..)
  , AuthBackend (..)
  , AuthError (..)
  
  , UserKey
  , Password
  , PwHash
  
  , login
  , logout
  , register
  , getCurrentUser
  , loginRequired
  , makePwHash
  , verifyPw
  ) where

import Control.Applicative ((<*>))
import Control.Monad (void)
import Control.Monad.IO.Class
import Crypto.PasswordStore
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding as T
import Data.Text.Encoding as ES

import Web.Beam
import Web.Beam.Types
import Web.Beam.Plugins.Session

import Web.Scotty.Trans (text)

type UserKey = Text
type Password = Text
type PwHash = Text

data AuthError = DuplicateUsername | UserDoesNotExist | InvalidPassword
  deriving (Show)

data AuthUser = AuthUser 
  { uniqueUserKey :: UserKey
  } deriving (Show)

data AuthContainer = forall r. AuthBackend r => AuthContainer {
      authStore :: r
    }

class SessionApp a => AuthApp a where
  getAuthContainer :: a -> AuthContainer

class AuthBackend c where
  backendLogin :: SessionApp a => UserKey -> Password -> c -> BeamActionM a (Either AuthError AuthUser)
  backendRegister :: UserKey -> Password -> c -> BeamActionM a (Either AuthError AuthUser)
  backendGetUser :: UserKey -> c -> BeamActionM a (Maybe AuthUser)
  backendLogout :: SessionApp a => c -> BeamActionM a ()
  backendLogout _ =  getUserSessionKey >>= deleteSessionValue

runWithContainer :: AuthApp a => 
                    (forall r. AuthBackend r => r -> BeamActionM a b) -> 
                    BeamActionM a b
runWithContainer f = do
  AuthContainer{..} <- gets getAuthContainer
  f authStore

getUserSessionKey :: BeamActionM a Text
getUserSessionKey = return $ T.pack "user-id" -- later read from settings.

register :: AuthApp a => UserKey -> Password -> BeamActionM a (Either AuthError AuthUser)
register un pw = runWithContainer $ backendRegister un pw

login :: AuthApp a => UserKey -> Password -> BeamActionM a (Either AuthError AuthUser)
login un pw = do
  loginResult <- (runWithContainer $ backendLogin un pw)
  case loginResult of
      Right au@(AuthUser userKey) -> do
          sessionKey <- getUserSessionKey
          deleteSessionValue sessionKey
          setSessionValue sessionKey userKey
      _ -> return ()
  return loginResult

logout :: AuthApp a => BeamActionM a ()
logout = runWithContainer $ backendLogout

getCurrentUser :: AuthApp a => BeamActionM a (Maybe AuthUser)
getCurrentUser = getUserSessionKey >>= getSessionValue' (T.pack "") >>=
                            (\uid -> runWithContainer $ backendGetUser uid)

loginRequired :: BeamActionM a () -> BeamActionM a ()
loginRequired action = action

makePwHash :: Password -> BeamActionM a PwHash
makePwHash pw =  fmap (T.fromStrict . ES.decodeUtf8) $ 
                        liftIO $ makePassword (ES.encodeUtf8 $ T.toStrict pw) 14

verifyPw :: Text -> Text -> Bool
verifyPw pw hash = verifyPassword (ES.encodeUtf8 $ T.toStrict pw) 
                          (ES.encodeUtf8 $ T.toStrict hash)