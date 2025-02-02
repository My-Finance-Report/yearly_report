{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes.Crud.Sankey.RegisterSankey (registerSankeyRoutes) where

import Auth (getCurrentUser, requireUser)
import ColumnChart (generateColChartData)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, splitOn, unpack)
import Data.Text.Lazy (fromStrict, toStrict)
import Database.Category (getCategoriesBySource, getCategory)
import Database.Configurations (addSankeyInput, addSankeyLinkage, getFirstSankeyConfig, removeSankeyInput, removeSankeyLinkage, saveSankeyConfig)
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
import HtmlGenerators.ConfigurationNew (renderConfigurationPageNew)
import HtmlGenerators.HtmlGenerators (renderSupportPage)
import Sankey (generateSankeyData)
import SankeyConfiguration (generateSankeyConfig)
import Types
import Web.Scotty (ActionM, ScottyM, formParam, formParams, get, header, html, json, post, redirect, setHeader, text)

registerSankeyRoutes :: ConnectionPool -> ScottyM ()
registerSankeyRoutes pool = do
  post "/add-sankey-linkage" $ requireUser pool $ \user -> do
    sankeyConfigIdText <- formParam "sankeyConfigId"
    linkageSourceCategory <- formParam "linkageSourceCategory"
    linkageTargetIdText <- formParam "linkageTargetId"

    let parseComposite keyValue =
          case splitOn "-" keyValue of
            [srcId, catId] -> Just (toSqlKey (read $ unpack srcId), toSqlKey (read $ unpack catId))
            _ -> Nothing

    case parseComposite linkageSourceCategory of
      Just (linkageSourceId, linkageCategoryId) -> do
        let linkageTargetId = toSqlKey (read $ unpack linkageTargetIdText)
            sankeyConfigId = toSqlKey (read $ unpack sankeyConfigIdText)

        liftIO $ addSankeyLinkage user sankeyConfigId linkageSourceId linkageCategoryId linkageTargetId

        referer <- header "Referer"
        let redirectTo = fromMaybe "/dashboard" referer
        redirect redirectTo
      Nothing -> text "Invalid linkage format"

  post "/remove-sankey-linkage" $ requireUser pool $ \user -> do
    sankeyConfigIdText <- formParam "sankeyConfigId"
    linkageSourceIdText <- formParam "inputSourceId"
    linkageCategoryIdText <- formParam "inputCategoryId"
    linkageTargetIdText <- formParam "targetSourceId"

    let sankeyConfigId = toSqlKey (read $ unpack sankeyConfigIdText)
    let linkageSourceId = toSqlKey (read $ unpack linkageSourceIdText)
    let linkageCategoryId = toSqlKey (read $ unpack linkageCategoryIdText)
    let linkageTargetId = toSqlKey (read $ unpack linkageTargetIdText)

    liftIO $ removeSankeyLinkage user sankeyConfigId linkageSourceId linkageCategoryId linkageTargetId

    referer <- header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer

    redirect redirectTo

  post "/add-sankey-input" $ requireUser pool $ \user -> do
    sankeyConfigIdText <- formParam "sankeyConfigId"
    inputSourceCategory <- formParam "inputSourceCategory"

    let parseComposite keyValue =
          case splitOn "-" keyValue of
            [srcId, catId] -> Just (toSqlKey (read $ unpack srcId), toSqlKey (read $ unpack catId))
            _ -> Nothing

    case parseComposite inputSourceCategory of
      Just (inputSourceId, inputCategoryId) -> do
        let sankeyConfigId = toSqlKey (read $ unpack sankeyConfigIdText)

        liftIO $ addSankeyInput user sankeyConfigId inputSourceId inputCategoryId

        referer <- header "Referer"
        let redirectTo = fromMaybe "/dashboard" referer
        redirect redirectTo
      Nothing -> text "Invalid input format"

  post "/remove-sankey-input" $ requireUser pool $ \user -> do
    sankeyConfigIdText <- formParam "sankeyConfigId"
    inputCategoryIdText <- formParam "inputCategoryId"
    inputSourceIdText <- formParam "inputSourceId"

    let inputCategoryId = toSqlKey (read $ unpack inputCategoryIdText)
    let inputSourceId = toSqlKey (read $ unpack inputSourceIdText)
    let sankeyConfigId = toSqlKey (read $ unpack sankeyConfigIdText)

    liftIO $ removeSankeyInput user sankeyConfigId inputSourceId inputCategoryId

    referer <- header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer
    redirect redirectTo

  post "/update-sankey-config" $ requireUser pool $ \user -> do
    allParams <- formParams

    let parseComposite keyValue =
          case splitOn "-" keyValue of
            [srcId, catId] -> Just (toSqlKey (read $ unpack srcId), toSqlKey (read $ unpack catId))
            _ -> Nothing

        inputPairs = [pair | (key, value) <- allParams, key == "inputSourceCategory[]", Just pair <- [parseComposite value]]

    linkageSourceCategory <- formParam "linkageSourceCategory"
    linkageTargetId <- formParam "linkageTargetId"

    let (linkageSourceId, linkageCategoryId) = fromMaybe (error "Invalid linkage format") (parseComposite linkageSourceCategory)

    -- Fetch database entities
    inputTransactionSources <- liftIO $ mapM (getTransactionSource user . fst) inputPairs
    inputCategories <- liftIO $ mapM (getCategory user . snd) inputPairs
    linkageSource <- liftIO $ getTransactionSource user linkageSourceId
    (linkageCategory, _) <- liftIO $ getCategory user linkageCategoryId
    linkageTarget <- liftIO $ getTransactionSource user (toSqlKey (read linkageTargetId))

    let rawInputs = zip inputTransactionSources inputCategories
        inputs = Prelude.map (\(source, (category, _)) -> (source, category)) rawInputs
        linkages = (linkageSource, linkageCategory, linkageTarget)
        newConfig = FullSankeyConfig {inputs = inputs, linkages = [linkages]}
    liftIO $ saveSankeyConfig user newConfig
    redirect "/dashboard"
