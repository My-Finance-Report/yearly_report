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

registerSankeyRoutes :: ConnectionPool -> ScottyM ()
registerSankeyRoutes pool = do
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

  