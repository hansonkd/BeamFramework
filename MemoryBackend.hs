module MemoryBackend where

import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Text.Lazy (Text)
import Data.Map as M
import Data.Maybe (fromMaybe, fromJust)

import Web.Beam
import Web.Beam.Types
import Web.Beam.Plugins.Auth
import Web.Beam.Plugins.Session

data SessionData = SessionData 
  { sessionMemory ::  TVar (M.Map Text (M.Map Text Text)) }
data UserData = UserData
  { userStorage :: TVar (M.Map UserKey PwHash) }

instance SessionBackend SessionData where
  backendSessionPut sessId key content (SessionData tv) =
      let insertFunc = (\sess -> 
                          Just $ M.insert key content (fromMaybe M.empty sess)
                       )
          tVarFunc = M.alter insertFunc sessId
      in liftIO $ atomically $ modifyTVar tv tVarFunc
  backendSessionGet sessId key (SessionData tv) = do
      curSessions <- liftIO $ readTVarIO tv
      return $ (M.lookup sessId curSessions) >>= (M.lookup key)
  backendSessionDelete sessId key (SessionData tv) =
      liftIO $ atomically $ modifyTVar tv (M.update (Just . (M.delete key)) sessId)
  backendSessionClear sessId (SessionData tv) =
      liftIO $ atomically $ modifyTVar tv (M.delete sessId)

instance AuthBackend UserData where
  backendGetUser name (UserData tv) = do
        possUser <- fmap (M.lookup name) $ liftIO $ readTVarIO tv
        case possUser of
          Nothing -> return Nothing
          Just _ -> return $ Just (AuthUser name)
  backendLogin name pw (UserData tv) = do
        users <- liftIO $ readTVarIO $ tv
        let possUser = M.lookup name users
            passCheck = fmap (verifyPw pw) possUser
        case passCheck of
            Just True -> return (Right $ AuthUser $ name)
            Just False -> return (Left InvalidPassword)
            Nothing -> return (Left UserDoesNotExist)
  backendRegister name pw (UserData tv) = do
        users <- liftIO $ readTVarIO $ tv
        if M.member name users
            then return (Left DuplicateUsername)
            else do
                pwHash <- makePwHash pw
                liftIO $ atomically $ writeTVar tv (M.insert name pwHash users)
                return (Right $ AuthUser name)

initSessionMemory :: BeamInitM a SessionContainer
initSessionMemory = do
  tv <- liftIO $ newTVarIO $ M.empty
  return $! SessionContainer {
    sessStore = SessionData tv
  }

initAuthMemory :: BeamInitM a AuthContainer
initAuthMemory = do
  tv <- liftIO $ newTVarIO $ M.empty
  return $! AuthContainer { 
    authStore = UserData tv
  }