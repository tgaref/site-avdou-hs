{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           RIO
import qualified RIO.Text as T
import           RIO.Process
import           System.Directory (setCurrentDirectory)
import           Options.Applicative
import           Avdou
import           Site
import           Types

commandParser :: Parser Options
commandParser = Options
  <$> switch
        (long "verbose"
         <> short 'v'
         <> help "Enable verbose mode")
  <*> hsubparser
        (command "build" (info (pure Build)
                          (progDesc "Build the site"))
         <> command "watch" (info (pure Watch)
                             (progDesc "Watch for changes and rebuild"))
         <> command "clean" (info (pure Clean)
                             (progDesc "Clean generated files"))
  )
  <*> option auto
        ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 8000
          <> showDefault
          <> help "Port for the local web server"
        )
  <*> strOption
        ( long "prefix"
          <> metavar "PREFIX"
          <> value ""
          <> showDefaultWith showPrefix
          <> help "URL prefix for generated site paths"
        )
  where
    showPrefix "" = "(none)"
    showPrefix s  = T.unpack s
  
opts :: ParserInfo Options
opts = info (commandParser <**> helper)
  (fullDesc
  <> progDesc "A static site generator based on Avdou-hs"
  <> header "site - generate a site")

main :: IO ()
main = do
  options <- execParser opts
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
 
  withLogFunc lo $ \lf -> do
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
    ref <- newIORef (Site "" "" mempty [] [])
    let siteEnv = SiteEnv ref app
    setCurrentDirectory "/home/tgaref/www/mysite/"
    runRIO siteEnv (unSiteM site)
    mysite <- readIORef ref
    case optionsCmd options of
      Build -> build mysite
      Watch -> watch mysite (optionsPort options)
      Clean -> clean mysite 
