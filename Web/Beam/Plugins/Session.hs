{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Web.Beam.Plugins.Session 
  ( SessionContainer (..)
  , SessionApp (..)
  , SessionBackend (..)
  
  , setSessionValue
  , getSessionValue
  , getSessionValue'
  , deleteSessionValue
  , generateSessionKey
  , getCurrentSessionKey
  , clearSessionKey
  ) where
    
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Encoding as T
import Data.UUID
import Data.UUID.V4

import Web.Beam.Types
import Web.Beam
import Web.Beam.Utils.Cookie

session_cookie_key = pack "-session-"

data SessionContainer = forall r. SessionBackend r => SessionContainer {
      sessStore :: r
    }

class SessionApp a where
  getSessionContainer :: a -> SessionContainer

class SessionBackend c where
  backendSessionPut :: Text -> Text -> Text -> c -> BeamActionM a ()
  backendSessionGet :: Text -> Text -> c -> BeamActionM a (Maybe Text)
  backendSessionDelete :: Text -> Text -> c -> BeamActionM a ()
  backendSessionClear :: Text -> c -> BeamActionM a ()

runWithContainer :: SessionApp a => (forall r. SessionBackend r => r -> BeamActionM a b) -> BeamActionM a b
runWithContainer f = do
  SessionContainer{..} <- gets getSessionContainer
  f sessStore

deleteSessionValue :: SessionApp a => Text -> BeamActionM a ()
deleteSessionValue key= do
      sessId <- getCurrentSessionKey 
      runWithContainer $ backendSessionDelete sessId key

setSessionValue :: SessionApp a => Text -> Text -> BeamActionM a ()
setSessionValue key content = do
      sessId <- getCurrentSessionKey 
      runWithContainer $ backendSessionPut sessId key content

getSessionValue :: SessionApp a => Text -> BeamActionM a (Maybe Text)
getSessionValue key = do
      sessId <- getCurrentSessionKey
      runWithContainer $ backendSessionGet sessId key

getSessionValue' :: SessionApp a => Text -> Text -> BeamActionM a Text
getSessionValue' def key = fmap (fromMaybe def) (getSessionValue key)
      
getSessionCookie :: BeamActionM a (Maybe Text)
getSessionCookie = getCookie session_cookie_key
    
generateSessionKey :: BeamActionM a Text
generateSessionKey = do
  newKey <- fmap (T.decodeUtf8 . toLazyASCIIBytes) (liftIO nextRandom)
  setCookie session_cookie_key newKey
  return newKey

getCurrentSessionKey :: BeamActionM a Text
getCurrentSessionKey = do
    curKey <- getSessionCookie
    case curKey of
      Just key -> return key
      Nothing -> generateSessionKey

clearSessionKey :: SessionApp a => BeamActionM a Text
clearSessionKey = do
    curKey <- getSessionCookie
    newKey <- generateSessionKey
    case curKey of
      Nothing -> return newKey
      Just oldKey -> do
          runWithContainer $ backendSessionClear oldKey
          return newKey