{-# LANGUAGE OverloadedStrings #-}

module Routes.Configuration.RegisterConfiguration (registerConfigurationRoutes) where

import Auth (getCurrentUser, requireUser)
import ColumnChart (generateColChartData)
import Control.Concurrent.Async (async)
import Control.Exception (SomeException (SomeException))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, splitOn, unpack)
import Data.Text.Lazy (fromStrict, toStrict)
import Database.Category (getCategoriesAndSources, getCategoriesBySource, getCategory)
import Database.Configurations (getFirstSankeyConfig, saveSankeyConfig)
import Database.Models (SourceKind (Account, Card, Investment), User (userOnboardingStep))
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql (ConnectionPool, toSqlKey)
import Database.Transaction (getAllTransactions, groupTransactionsBySource)
import Database.TransactionSource (getAllTransactionSources, getTransactionSource)
import Database.UploadConfiguration (getAllUploadConfigs)
import HtmlGenerators.AccountManagement (renderAccountManagement)
import HtmlGenerators.ConfigurationNew (renderConfigurationPageNew)
import HtmlGenerators.HtmlGenerators (renderSupportPage)
import HtmlGenerators.Layout (renderPage)
import Sankey (generateSankeyData)
import Types
import Web.Scotty (ActionM, ScottyM, formParam, formParams, get, html, json, post, redirect, setHeader)

registerConfigurationRoutes :: ConnectionPool -> ScottyM ()
registerConfigurationRoutes pool = do
  get "/new-configuration" $ requireUser pool $ \user -> do
    uploaderConfigs <- liftIO $ getAllUploadConfigs user
    transactionSources <- liftIO $ getAllTransactionSources user
    (sankeyConfig, configId) <- liftIO $ getFirstSankeyConfig user

    categoriesBySource <- liftIO $ do
      categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
      return $ Map.fromList $ zip transactionSources categories

    let content = renderConfigurationPageNew configId sankeyConfig categoriesBySource uploaderConfigs transactionSources
    html $ renderPage (Just user) "Configuration" content True

  get "/manage-accounts" $ requireUser pool $ \user -> do
    categoriesBySource <- liftIO $ getCategoriesAndSources user

    let content = renderAccountManagement user [Account, Card, Investment] categoriesBySource True
    Web.Scotty.html $ renderPage (Just user) "Account Management" content True