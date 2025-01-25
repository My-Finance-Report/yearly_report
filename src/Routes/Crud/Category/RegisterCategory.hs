{-# LANGUAGE OverloadedStrings #-}

module Routes.Crud.Category.RegisterCategory (registerCategoryRoutes) where

import Auth (getCurrentUser, requireUser)
import ColumnChart (generateColChartData)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, splitOn, unpack)
import Data.Text.Lazy (fromStrict, toStrict)
import Database.Category (addCategory, getCategoriesBySource, getCategory, removeCategory, updateCategory)
import Database.Configurations (getFirstSankeyConfig, saveSankeyConfig)
import Database.Database (updateUserOnboardingStep)
import Database.Models (Category (Category), User (userOnboardingStep))
import Database.Persist
import Database.Persist.Postgresql (ConnectionPool, toSqlKey)
import Database.Transaction (getAllTransactions, groupTransactionsBySource, updateTransactionCategory)
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
import Web.Scotty (ActionM, ScottyM, formParam, formParams, get, header, html, json, pathParam, post, redirect)

registerCategoryRoutes :: ConnectionPool -> ScottyM ()
registerCategoryRoutes pool = do
  post "/update-category" $ requireUser pool $ \user -> do
    tId <- formParam "transactionId"
    newCat <- formParam "newCategory"
    let newCatId = toSqlKey (read $ unpack newCat) :: Key Category
    fileArg <- formParam "filename"
    liftIO $ updateTransactionCategory user (read $ unpack tId) newCatId
    redirect $ fromStrict ("/transactions/" <> fileArg)

  post "/add-category/:sourceId" $ requireUser pool $ \user -> do
    sourceIdText <- pathParam "sourceId"
    let sourceId = toSqlKey $ read sourceIdText
    newCategory <- formParam "newCategory" :: ActionM Text
    liftIO $ addCategory user newCategory sourceId

    referer <- header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer

    redirect redirectTo

  post "/remove-category/:catId" $ requireUser pool $ \user -> do
    catIdText <- pathParam "catId"
    let catId = toSqlKey $ read catIdText
    liftIO $ removeCategory user catId

    referer <- header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer

    redirect redirectTo

  post "/edit-category/:id" $ requireUser pool $ \user -> do
    catIdText <- Web.Scotty.pathParam "id"
    let catId = toSqlKey $ read catIdText
    catName <- Web.Scotty.formParam "updatedCategoryName"
    liftIO $ updateCategory user catId catName
    referer <- header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer
    redirect redirectTo
