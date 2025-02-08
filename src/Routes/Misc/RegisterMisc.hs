{-# LANGUAGE OverloadedStrings #-}

module Routes.Misc.RegisterMisc (registerMiscRoutes) where

import Auth (getCurrentUser, requireUser)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef)
import qualified Data.Map
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, toStrict)
import Database.Category (getCategoriesBySource)
import Database.Configurations (saveSankeyConfig)
import Database.Database (updateUserOnboardingStep)
import Database.Jobs
import Database.Models
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql (ConnectionPool)
import Database.UploadConfiguration (getAllUploadConfigs)
import HtmlGenerators.Components (makeSimpleBanner)
import HtmlGenerators.HomePage (renderHomePage)
import HtmlGenerators.HtmlGenerators (renderSupportPage)
import HtmlGenerators.LandingPage (renderLandingPage)
import HtmlGenerators.Layout (renderPage)
import Web.Scotty (ActionM, ScottyM, formParam, get, html, post, redirect, setHeader)

registerMiscRoutes :: ConnectionPool -> ScottyM ()
registerMiscRoutes pool = do
  get "/" $ do
    user <- getCurrentUser pool
    case user of
      Just user -> redirect "/dashboard"
      Nothing -> html $ renderPage user "My Financial Report" renderLandingPage False

  get "/help" $ do
    user <- getCurrentUser pool
    html $ renderPage user "Help Me" renderSupportPage False

  get "/dashboard" $ requireUser pool $ \user -> do
    pendingJobs <- getFileJobsCount user Pending
    processingJobs <- getFileJobsCount user Processing
    let banner = if (pendingJobs + processingJobs) > 0 then Just $ makeSimpleBanner "Processing transactions, check back soon!" else Nothing
    content <- liftIO $ renderHomePage user banner
    Web.Scotty.html $ renderPage (Just user) "Financial Summary" content True