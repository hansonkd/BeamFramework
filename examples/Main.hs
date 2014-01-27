{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String
import Data.Text.Lazy (Text)
import Data.Monoid ((<>))

import Web.Scotty.Trans

import Web.Beam
import Web.Beam.Types
import Web.Beam.Plugins.Auth
import Web.Beam.Plugins.Session
import Web.Beam.Utils.Cookie

-- | Use the RAM auth and session backend. Later we could swap this out
-- for a DB backend w/o having to change our application code.
import MemoryBackend

type MyApp b = BeamApplication AppState b

-- | Our Application datatype must hold thread safe resources for our plugins.
data AppState = AppState { authContainer :: AuthContainer
                         , sessContainer :: SessionContainer }

-- | Allow plugins to know where their resources are.
instance AuthApp AppState where
  getAuthContainer = authContainer

instance SessionApp AppState where
  getSessionContainer = sessContainer
  
main :: IO ()
main = do
  -- Initialize plugin resources and settings. Plugins can add settings and
  -- additional functions (e.g. routes and middleware) that will run before
  -- your application.
  (initState, appConfig) <- runBeamInit $ do
      authMem <- initAuthMemory
      sessMem <- initSessionMemory
      return $ AppState authMem sessMem
  runBeamApp appConfig initState app

app :: MyApp ()
app = do
    get "/" $ do
        c <- getCurrentUser
        html $ "<html><body>" <> (fromString $ show c) <> "</body></html>"
    get "/register" $ do
        register "bob" "123"
        redirect "/"
    get "/login" $ do
        c <- login "bob" "123"
        html $ "<html><body>" <> (fromString $ show c) <> "</body></html>"
    get "/logout" $ do
        logout
        redirect "/"