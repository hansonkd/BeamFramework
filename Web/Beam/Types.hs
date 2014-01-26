{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Beam.Types where

import Control.Concurrent.STM
import Control.Monad.Reader 
import Control.Monad.Writer.Strict
import Data.Default
import Data.Text.Lazy (Text)
import Data.Map.Strict as M
import Data.Monoid

import Web.Scotty.Trans

data BeamInternal a = BeamInternal 
  { beamSettings :: M.Map Text Text
  , coreApplication :: a 
  }

data BeamInitData a = BeamInitData
  { initSteps :: [BeamApplication a ()]
  , initSettings :: M.Map Text Text
  }

data BeamConfig a = BeamConfig
  { appSteps :: [BeamApplication a ()]
  , appSettings ::  M.Map Text Text
  , scottyOptions :: Options
  }

instance Default (BeamInitData a) where
    def = BeamInitData def def

instance Default (BeamConfig a) where
    def = BeamConfig def def def

instance Monoid (BeamInitData a) where
  mempty = def
  mappend (BeamInitData ap1 as1) (BeamInitData ap2 as2) = 
      BeamInitData (ap1 <> ap2) (as1 <> as2)

newtype BeamInitM a b = BeamInitM { runBeamInitM :: WriterT (BeamInitData a) IO b }
    deriving (Monad, MonadIO, MonadWriter (BeamInitData a), Functor)

newtype BeamM a b = BeamM { runBeamM :: ReaderT (TVar (BeamInternal a)) IO b }
    deriving (Monad, MonadIO, MonadReader (TVar (BeamInternal a)), Functor)

type BeamApplication a b = ScottyT Text (BeamM a) b
type BeamActionM a b = ActionT Text (BeamM a) b