{-# LANGUAGE OverloadedStrings #-}

module Routes.Api.Visualization.RegisterVisualization (registerVisualizationRoutes) where

import Auth (getCurrentUser, requireUser)
import ColumnChart (generateColChartData)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, toStrict)
import Database.Category (getCategoriesBySource)
import Database.Configurations (getFirstSankeyConfig, saveSankeyConfig)
import Database.Database (updateUserOnboardingStep)
import Database.Models (User (userOnboardingStep))
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql (ConnectionPool)
import Database.Transaction (getAllTransactions, groupTransactionsBySource)
import Database.UploadConfiguration (getAllUploadConfigs)
import Sankey (generateSankeyData)
import SankeyConfiguration (generateSankeyConfig)
import Web.Scotty (ActionM, ScottyM, formParam, get, json, post, redirect, setHeader)

registerVisualizationRoutes :: ConnectionPool -> ScottyM ()
registerVisualizationRoutes pool = do
  get "/api/sankey-data" $ requireUser pool $ \user -> do
    categorizedTransactions <- liftIO $ getAllTransactions user
    gbs <- groupTransactionsBySource user categorizedTransactions

    (sankeyConfig, _) <- liftIO $ getFirstSankeyConfig user
    json $ generateSankeyData gbs sankeyConfig

  get "/api/column-data" $ requireUser pool $ \user -> do
    transactions <- liftIO $ getAllTransactions user
    columnData <- generateColChartData user transactions
    json columnData