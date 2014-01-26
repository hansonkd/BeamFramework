module Web.Beam 
  ( gets
  , modify
  , getSetting
  , runBeamApp
  , runBeamInit
  ) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Default
import Data.Text.Lazy (Text)
import Data.Map.Strict as M

import Network.Wai.Middleware.RequestLogger

import Web.Scotty.Trans

import Web.Beam.Types


beamM :: MonadTrans t => BeamM a b -> t (BeamM a) b
beamM = lift

gets :: (a -> b) -> BeamActionM a b
gets f = beamM $ ask >>= liftIO . readTVarIO >>= return . f . coreApplication

modify :: (a -> a) -> BeamActionM a ()
modify f = beamM $ ask >>= 
    liftIO . atomically . flip modifyTVar' (wrapInternal f)
  where wrapInternal func internalData =
         internalData { coreApplication = f (coreApplication internalData)} 

getSetting :: MonadTrans t => Text -> t (BeamM a) (Maybe Text)
getSetting k = beamM $ ask >>= liftIO . readTVarIO >>= return . f . beamSettings
  where f = M.lookup k

runBeamInit :: BeamInitM a b -> IO (b, BeamConfig a)
runBeamInit m = do
    (initAppState, BeamInitData paths settings) <- runWriterT (runBeamInitM m)
    return $ (initAppState, def {appSteps = paths, appSettings = settings})

runBeamApp :: BeamConfig a -> a -> BeamApplication a () -> IO ()
runBeamApp bConfig initState bApp = do
    sync <- newTVarIO $ BeamInternal (appSettings bConfig) initState
    let runM m = runReaderT (runBeamM m) sync
        runActionToIO = runM
    scottyOptsT (scottyOptions bConfig) runM runActionToIO $
                (middleware logStdoutDev) >> (sequence (appSteps bConfig)) >> bApp