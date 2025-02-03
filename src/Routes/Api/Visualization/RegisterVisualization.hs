{-# LANGUAGE OverloadedStrings #-}

module Routes.Api.Visualization.RegisterVisualization (registerVisualizationRoutes) where

import Auth (getCurrentUser, requireUser)
import ColumnChart (generateColChartData)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, toStrict, unpack)
import Database.Category (getCategoriesBySource)
import Database.Configurations (getFirstSankeyConfig, saveSankeyConfig)
import Database.Database (updateUserOnboardingStep)
import Database.Models (Category (categorySourceId), User (userOnboardingStep))
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql (ConnectionPool, toSqlKey)
import Database.Transaction (getAllTransactions, groupTransactionsBySource)
import Database.TransactionSource (getAllTransactionSources)
import Database.UploadConfiguration (getAllUploadConfigs)
import Sankey (generateSankeyData)
import SankeyConfiguration (generateSankeyConfig)
import Text.Read (readMaybe)
import Types
import Web.Scotty (ActionM, ScottyM, get, json, post, queryParam, queryParamMaybe, redirect, setHeader)

registerVisualizationRoutes :: ConnectionPool -> ScottyM ()
registerVisualizationRoutes pool = do
  get "/api/sankey-data" $ requireUser pool $ \user -> do
    categorizedTransactions <- liftIO $ getAllTransactions user
    gbs <- groupTransactionsBySource user categorizedTransactions

    (sankeyConfig, _) <- liftIO $ getFirstSankeyConfig user
    json $ generateSankeyData gbs sankeyConfig

  get "/api/column-data" $ requireUser pool $ \user -> do
    transactions <- liftIO $ getAllTransactions user

    mSourceIdText <- queryParamMaybe "sourceId"
    let mSourceId =
          mSourceIdText >>= \idText ->
            case readMaybe (unpack idText) of
              Just intId -> Just (toSqlKey (fromIntegral intId))
              Nothing -> Nothing

    columnData <- generateColChartData user transactions mSourceId
    json columnData