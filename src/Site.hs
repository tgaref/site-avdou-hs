{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Site (site) where

import           Prelude (print)
import           RIO
import qualified RIO.Map as Map
import qualified RIO.Text as T
import           RIO.FilePath ((</>))
import qualified RIO.Time as Time
import           RIO.HashMap (foldlWithKey')
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Aeson as Aeson
import           Types (App(..), Options(optionsPrefix))
import           Avdou

site :: SiteM (RIO App) ()
site = do
  let siteDir = "/home/tgaref/www/site-avdou-hs/"
      publicDir = "/home/tgaref/www/site-avdou-hs/public/"

  setSiteDir siteDir
  setPublicDir publicDir
  
  ts <- liftIO $ loadTemplates "templates"

  setTemplates ts 

  ctx <- lift context

  copy "css/*" idRoute
  copy "content/static/**/*" idRoute
  copy ("content/teaching/**/*" .\\. "content/teaching/**/*.md") idRoute
  copy ("content/research/**/*" .\\. "content/research/**/*.md") idRoute
  copy ("content/activities/**/*" .\\. "content/activities/**/*.md") idRoute

  match "content/index.md" $ do
    applyCompiler markdownCompiler
    applyTemplate "index.html" ctx
    applyTemplate "base.html" ctx
    routeTo (const $ publicDir </> "index.html")

  match ("content/*.md" .\\. "content/index.md") $ do
    applyCompiler (shortcodeCompiler shortcodes)
    applyCompiler markdownCompiler
    applyTemplate "section.html" ctx
    applyTemplate "base.html" ctx
    routeTo niceRoute

  match "content/teaching/*/*.md" $ do
    applyCompiler (shortcodeCompiler shortcodes)
    applyCompiler markdownCompiler
    applyTemplate "course.html" ctx
    applyTemplate "base.html" ctx
    routeTo niceRoute

  match "content/activities/*/index.md" $ do
    applyCompiler markdownCompiler
    applyTemplate "section.html" ctx
    applyTemplate "base.html" ctx
    routeTo (setExtension "html")

  match "content/activities/cryptostudygroup/*.md" $ do
    applyCompiler (shortcodeCompiler sectioncode)
    applyCompiler markdownCompiler
    applyTemplate "study.html" ctx
    applyTemplate "base.html" ctx
    routeTo niceRoute

  match "content/activities/cryptostudygroup/*.html" $ do
    getMetadata False
    applyTemplate "slides-base.html" ctx
    routeTo idRoute


{-
setup :: RIO App Site
setup  = do
  let siteDir = "/home/tgaref/www/site-avdou-hs/"
      publicDir = "/home/tgaref/www/site-avdou-hs/public/"   
  ts <- liftIO $ loadTemplates "templates"
  ctx <- context


  let m = Mine (Simple "content/*.md") [getTitle] True

  d <- executeMine siteDir mine

  -- the above is equivalent to this:

  d <- mine siteDir "content/*.md" $ do
        applyMiner getTitle

  liftIO $ print $ genList d


  let copies = [ Copy (Simple "css/*") idRoute
               , Copy (Simple "content/static/**/*") idRoute
               , Copy (Diff "content/teaching/**/*" "content/teaching/**/*.md") idRoute
               , Copy (Diff "content/research/**/*" "content/research/**/*.md") idRoute
               , Copy (Diff "content/activities/**/*" "content/activities/**/*.md") idRoute
               ]

  let rules = [ Rule (Simple "content/index.md") [markdownCompiler] [("index.html", ctx), ("base.html", ctx)] (const $ publicDir </> "index.html") True 
              , Rule (Diff "content/*.md" "content/index.md") [markdownCompiler] [("section.html", ctx), ("base.html", ctx)] niceRoute True 
              , Rule (Simple "content/teaching/*/*.md") [shortcodeCompiler shortcodes, markdownCompiler] [("course.html", ctx), ("base.html", ctx)] niceRoute True 
              , Rule (Simple "content/activities/*/index.md") [markdownCompiler] [("section.html", ctx), ("base.html", ctx)] (setExtension "html") True

              ]
  let site = Site { _siteDir = siteDir
                  , _publicDir = publicDir 
                  , _templates = ts
                  , _copies = copies
                  , _rules = rules
                  }
  pure site
-}

---------------------------------------------------------
--- Example how to produce some Context given
--- the output of executeMine
---------------------------------------------------------

genList :: HashMap Text Context -> Context
genList h =
  foldlWithKey' (\ctx fp v ->
                   case lookupCtx "title" v of
                     Just t -> insertCtx fp t ctx
                     _      -> ctx
                ) mempty h


---------------------------------------------------------
--- Miners
---------------------------------------------------------

getTitle :: Document -> Context
getTitle doc =
   case lookupCtx "title" (view docMetaL doc) of
     Just o -> insertCtx "title" o mempty
     _      -> mempty
      

---------------------------------------------------------
--- Context 
---------------------------------------------------------

context :: RIO App Context
context = do
  app <- ask
  let prefix = optionsPrefix . appOptions $ app 

  nowUTC <- Time.getCurrentTime
  tz <- Time.getCurrentTimeZone
  let localTime = Time.utcToLocalTime tz nowUTC
  let formattedTime = T.pack $ formatTime defaultTimeLocale "%d %B %Y" localTime
  pure $ insertManyCtx [("prefix", Aeson.String prefix), ("today", Aeson.String formattedTime)]  mempty 



---------------------------------------------------------
--- Shortcodes
---------------------------------------------------------
shortcodes :: ShortcodeConfig
shortcodes = Map.fromList
        [ ("calitem", calitemExpander)
        , ("pubitem", pubitemExpander)
        ]

sectioncode :: ShortcodeConfig
sectioncode = Map.fromList
              [ ("sectionitem", sectionitemExpander)
              ]

calitemExpander :: [Text] -> Text
calitemExpander args =
  case args of 
    [date, content] -> T.concat
      [ "<div class=\"box calendar-entry\">"
      , "<p>"
      ,  "\n<div x-data=\"{ open: false }\">\n"
      , "<a x-on:click=\"open = ! open\"><strong>", date, "</strong></a>\n"
      , "<br><br>\n"
      , "<div x-show=\"open\"> \n"
      , content
      , "</div>\n</div>\n</p>\n</div>"
      ]
    _ -> error $ "\\calitem requires 2 arguments, got " <> show (length args)

pubitemExpander :: [Text] -> Text
pubitemExpander args =
  case args of
    [title, authors, journal] -> T.concat
      [ "<div class=\"cell\">"
      , title
      , "\n<br/>"
      , authors
      , "\n<br/>"
      , journal
      , "\n<br><br>\n"
      , "</div>"
      ]
    [title, authors, journal, links] -> T.concat
      [ "<div class=\"cell\">"
      , title
      , "\n<br/>"
      , authors
      , "\n<br/>"
      , journal
      , "\n<br/>"
      , links 
      , "\n<br><br>\n"
      , "</div>"
      ]
    _ -> error $ "\\pubitem requires 3 or 4 arguments, got " <> show (length args)

sectionitemExpander :: [Text] -> Text
sectionitemExpander args =
  case args of 
    [level, title, content] -> T.concat
      [ "<div class=\"box calendar-entry\">"
      , "<p>"
      ,  "\n<div x-data=\"{ open: false }\">\n"
      , "<a x-on:click=\"open = ! open\">"
      , "<h", level,"> "
      , title
      , "</h", level,">"
      , "\n</a>\n"
      , "<div x-show=\"open\">"
      , content
      , "</div>\n</div>\n</p>\n</div>"
      ]
    _ -> error $ "\\calitem requires 2 arguments, got " <> show (length args)
