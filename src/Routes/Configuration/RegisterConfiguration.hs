{-# LANGUAGE OverloadedStrings #-}

module Routes.Configuration.RegisterConfiguration (registerConfigurationRoutes) where

import Auth (getCurrentUser, requireUser)
import ColumnChart (generateColChartData)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, splitOn, unpack)
import Data.Text.Lazy (fromStrict, toStrict)
import Database.Category (getCategoriesBySource, getCategory, getCategoriesAndSources)
import Database.Configurations (getFirstSankeyConfig, saveSankeyConfig)
import Database.Database (updateUserOnboardingStep)
import Database.Models (User (userOnboardingStep))
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql (ConnectionPool, toSqlKey)
import Database.Transaction (getAllTransactions, groupTransactionsBySource)
import Database.TransactionSource (getAllTransactionSources, getTransactionSource)
import Database.UploadConfiguration (getAllUploadConfigs)
import HtmlGenerators.AuthPages (renderLoginPage)
import HtmlGenerators.ConfigurationNew (renderConfigurationPageNew)
import HtmlGenerators.HomePage (makeSimpleBanner, renderHomePage)
import HtmlGenerators.HtmlGenerators (renderSupportPage)
import HtmlGenerators.LandingPage (renderLandingPage)
import HtmlGenerators.Layout (renderPage)
import Sankey (generateSankeyData)
import SankeyConfiguration (generateSankeyConfig)
import Types
import Web.Scotty (ActionM, ScottyM, formParam, formParams, get, html, json, post, redirect, setHeader)
import HtmlGenerators.Configuration (renderConfigurationPage)
import HtmlGenerators.AccountManagement (renderAccountManagement)

registerConfigurationRoutes :: ConnectionPool -> ScottyM ()
registerConfigurationRoutes pool = do

    get "/configuration" $ requireUser pool $ \user -> do
        uploaderConfigs <- liftIO $ getAllUploadConfigs user
        transactionSources <- liftIO $ getAllTransactionSources user
        sankeyConfig <- liftIO $ getFirstSankeyConfig user
        categoriesBySource <- liftIO $ do
            categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
            return $ Map.fromList $ zip transactionSources categories

        Web.Scotty.html $ renderPage (Just user) "Configuration" $ renderConfigurationPage sankeyConfig categoriesBySource uploaderConfigs transactionSources

    get "/new-configuration" $ requireUser pool $ \user -> do
        uploaderConfigs <- liftIO $ getAllUploadConfigs user
        transactionSources <- liftIO $ getAllTransactionSources user
        sankeyConfig <- liftIO $ getFirstSankeyConfig user
        categoriesBySource <- liftIO $ do
            categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
            return $ Map.fromList $ zip transactionSources categories

        html $ renderPage (Just user) "Configuration" $ renderConfigurationPageNew sankeyConfig categoriesBySource uploaderConfigs transactionSources

    get "/manage-accounts" $ requireUser pool $ \user -> do
        categoriesBySource <- liftIO $ getCategoriesAndSources user
        let content = renderAccountManagement user categoriesBySource True
        Web.Scotty.html $ renderPage (Just user) "Account Management" content