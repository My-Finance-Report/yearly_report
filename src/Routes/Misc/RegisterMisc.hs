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
import Database.Models (User (userOnboardingStep))
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
import SankeyConfiguration (generateSankeyConfig)
import Web.Scotty (ActionM, ScottyM, formParam, get, html, post, redirect, setHeader)

registerMiscRoutes :: ConnectionPool -> IORef Int -> ScottyM ()
registerMiscRoutes pool activeJobs = do
  get "/" $ do
    user <- getCurrentUser pool
    case user of
      Just user -> redirect "/dashboard"
      Nothing -> html $ renderPage user "My Financial Report" renderLandingPage False

  get "/help" $ do
    user <- getCurrentUser pool
    html $ renderPage user "Help Me" renderSupportPage False

  get "/dashboard" $ requireUser pool $ \user -> do
    let onboardingStep = userOnboardingStep $ entityVal user
    case onboardingStep of
      Just _ -> Web.Scotty.redirect "/onboarding"
      Nothing -> do
        activeJobs <- liftIO $ readIORef activeJobs
        let banner = if activeJobs > 0 then Just $ makeSimpleBanner "Processing transactions, check back soon!" else Nothing
        content <- liftIO $ renderHomePage user banner
        Web.Scotty.html $ renderPage (Just user) "Financial Summary" content True