{-# LANGUAGE OverloadedStrings #-}

module Routes.Onboarding.RegisterOnboarding (registerOnboardingRoutes) where

import Auth
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map
import Database.Category (getCategoriesBySource)
import Database.Configurations (saveSankeyConfig)
import Database.Database (updateUserOnboardingStep)
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
  -- Main onboarding route (redirects based on current step)
  get "/onboarding" $ requireUser pool $ \user -> do
    let currentStep = userOnboardingStep $ entityVal user
    case currentStep of
      Just 0 -> redirect "/onboarding/step-1"
      Just 1 -> redirect "/onboarding/step-2"
      _ -> redirect "/dashboard"

  -- Step 1 of onboarding
  get "/onboarding/step-1" $ requireUser pool $ \user -> do
    transactionSources <- liftIO $ getAllTransactionSources user
    let content = renderOnboardingOne user transactionSources True
    Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

  post "/onboarding/step-2" $ requireUser pool $ \user -> do
    liftIO $ updateUserOnboardingStep user Nothing
    redirect "/onboarding/step-2"

  post "/onboarding/step-1" $ requireUser pool $ \user -> do
    liftIO $ updateUserOnboardingStep user (Just 1)
    redirect "/onboarding/step-2"

  get "/onboarding/step-2" $ requireUser pool $ \user -> do
    liftIO $ print "we are in step 2"
    transactionSources <- liftIO $ getAllTransactionSources user
    categoriesBySource <- liftIO $ do
      categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
      return $ Data.Map.fromList $ zip transactionSources categories

    let content = renderOnboardingTwo user categoriesBySource True
    Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

  get "/add-account/step-3" $ requireUser pool $ \user -> do
    transactionSources <- liftIO $ getAllTransactionSources user
    uploadConfigs <- liftIO $ getAllUploadConfigs user
    let content = renderOnboardingThree user transactionSources (map entityVal uploadConfigs) False
    Web.Scotty.html $ renderPage (Just user) "User Onboarding" content

  post "/onboarding/finalize" $ requireUser pool $ \user -> do
    transactionSources <- liftIO $ getAllTransactionSources user
    categoriesBySource <- liftIO $ do
      categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
      return $ Data.Map.fromList $ zip transactionSources categories

    liftIO $ do
      void $ async $ do
        config <- generateSankeyConfig user categoriesBySource
        case config of
          Just con -> do
            saveSankeyConfig user con
            return ()
          Nothing -> putStrLn "Error: Failed to generate Sankey configuration."
    liftIO $ updateUserOnboardingStep user Nothing
    Web.Scotty.redirect "/dashboard"
