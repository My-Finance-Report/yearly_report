{-# LANGUAGE OverloadedStrings #-}

module Routes.Onboarding.RegisterOnboarding (registerOnboardingRoutes) where

import Auth
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map
import Database.Category (getCategoriesAndSources, getCategoriesBySource)
import Database.Configurations (saveSankeyConfig)
import Database.Models
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql (ConnectionPool)
import Database.TransactionSource
import Database.UploadConfiguration (getAllUploadConfigs)
import HtmlGenerators.Layout (renderPage)
import HtmlGenerators.OnboardingFour (renderOnboardingFour)
import HtmlGenerators.OnboardingOne
import HtmlGenerators.OnboardingThree (renderOnboardingThree)
import HtmlGenerators.OnboardingTwo (renderOnboardingTwo)
import SankeyConfiguration (generateSankeyConfig)
import Web.Scotty (ActionM, ScottyM, get, html, post, redirect)

registerOnboardingRoutes :: ConnectionPool -> ScottyM ()
registerOnboardingRoutes pool = do
  post "/onboarding/finalize" $ requireUser pool $ \user -> do
    categoriesBySource <- liftIO $ getCategoriesAndSources user

    liftIO $ do
      void $ async $ do
        config <- generateSankeyConfig user categoriesBySource
        case config of
          Just con -> do
            saveSankeyConfig user con
            return ()
          Nothing -> putStrLn "Error: Failed to generate Sankey configuration."
    Web.Scotty.redirect "/dashboard"
