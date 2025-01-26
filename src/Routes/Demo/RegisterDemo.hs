{-# LANGUAGE OverloadedStrings #-}

module Routes.Demo.RegisterDemo (registerDemoRoutes) where

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
import Database.Database (getDemoUser, updateUserOnboardingStep)
import Database.Models (User (userOnboardingStep))
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql (ConnectionPool)
import Database.Transaction (getAllTransactions, groupTransactionsBySource)
import Database.UploadConfiguration (getAllUploadConfigs)
import HtmlGenerators.HomePage (makeDemoBanner, makeSimpleBanner, renderHomePage)
import HtmlGenerators.HtmlGenerators (renderSupportPage)
import HtmlGenerators.Layout (renderPage)
import Sankey (generateSankeyData)
import SankeyConfiguration (generateSankeyConfig)
import Web.Scotty (ActionM, ScottyM, formParam, get, html, json, post, redirect)

registerDemoRoutes :: ConnectionPool -> ScottyM ()
registerDemoRoutes pool = do
  get "/demo/api/sankey-data" $ do
    user <- getDemoUser
    categorizedTransactions <- liftIO $ getAllTransactions user
    gbs <- groupTransactionsBySource user categorizedTransactions

    (config, _) <- liftIO $ getFirstSankeyConfig user

    json (generateSankeyData gbs config)

  get "/demo/api/histogram-data" $ do
    user <- getDemoUser
    transactions <- liftIO $ getAllTransactions user
    histogramData <- generateColChartData user transactions
    json histogramData

  get "/demo-account" $ do
    demoUser <- getDemoUser
    content <- liftIO $ renderHomePage demoUser (Just makeDemoBanner)
    html $ renderPage (Just demoUser) "Financial Summary" content False