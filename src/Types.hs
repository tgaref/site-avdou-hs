{-# LANGUAGE NoImplicitPrelude #-}

module Types
  ( App (..)
  , Options (..)
  , Command (..)
  , HasApp (..)
  , SiteEnv (..)
  ) where

import RIO
import RIO.Process
import Avdou.Types

data Command = Build 
             | Watch
             | Clean
             deriving (Eq, Show)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsCmd     :: !Command
  , optionsPort    :: !Int
  , optionsPrefix  :: !Text
  } deriving Show

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

class HasApp env where
  appL :: Lens' env App

instance HasApp App where
  appL = id

data SiteEnv = SiteEnv { _siteEnvRef :: IORef Site, _siteEnvApp :: App }

instance HasSiteRef SiteEnv where
  siteRefL = lens _siteEnvRef (\e r -> e {_siteEnvRef = r})

instance HasApp SiteEnv where
  appL = lens _siteEnvApp (\e a -> e {_siteEnvApp = a})
