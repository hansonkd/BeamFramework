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

import MemoryBackend

type MyApp b = BeamApplication AppState b

data AppState = AppState { tickCount :: Int
                         , authContainer :: AuthContainer
                         , sessContainer :: SessionContainer }
                         
instance AuthApp AppState where
  getAuthContainer = authContainer

instance SessionApp AppState where
  getSessionContainer = sessContainer
  
main :: IO ()
main = do
  (initState, appConfig) <- runBeamInit $ do
      authMem <- initAuthMemory
      sessMem <- initSessionMemory
      return $ AppState 0 authMem sessMem
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
        
    get "/plusone" $ do
        modify $ \ st -> st { tickCount = tickCount st + 1 }
        redirect "/"
    get "/plustwo" $ do
        modify $ \ st -> st { tickCount = tickCount st + 2 }
        redirect "/"