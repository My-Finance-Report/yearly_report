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
import Database.Database (updateUserOnboardingStep, getDemoUser)
import Database.Models (User (userOnboardingStep))
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql (ConnectionPool)
import Database.Transaction (getAllTransactions, groupTransactionsBySource)
import Database.UploadConfiguration (getAllUploadConfigs)
import HtmlGenerators.AuthPages (renderLoginPage)
import HtmlGenerators.HomePage (makeSimpleBanner, renderHomePage, makeDemoBanner)
import HtmlGenerators.HtmlGenerators (renderSupportPage)
import HtmlGenerators.LandingPage (renderLandingPage)
import HtmlGenerators.Layout (renderPage)
import Sankey (generateSankeyData)
import SankeyConfiguration (generateSankeyConfig)
import Web.Scotty (ActionM, ScottyM, formParam, get, json, post, redirect,  html)

registerDemoRoutes :: ConnectionPool -> ScottyM ()
registerDemoRoutes pool = do

    get "/demo/api/sankey-data" $ do
      user <- getDemoUser
      categorizedTransactions <- liftIO $ getAllTransactions user
      gbs <- groupTransactionsBySource user categorizedTransactions

      sankeyConfig <- liftIO $ getFirstSankeyConfig user
      let sankeyData = case sankeyConfig of
            Just config -> Just (generateSankeyData gbs config)
            Nothing -> Nothing

      json sankeyData

    get "/demo/api/histogram-data" $ do
      user <- getDemoUser
      transactions <- liftIO $ getAllTransactions user
      histogramData <- generateColChartData user transactions
      json histogramData


    get "/demo-account" $ do
        demoUser <- getDemoUser
        content <- liftIO $ renderHomePage demoUser (Just makeDemoBanner)
        html $ renderPage (Just demoUser) "Financial Summary" content