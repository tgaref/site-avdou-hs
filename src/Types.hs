{-# LANGUAGE NoImplicitPrelude #-}

module Types
  ( App (..)
  , Options (..)
  , Command (..)
  ) where

import RIO
import RIO.Process

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
