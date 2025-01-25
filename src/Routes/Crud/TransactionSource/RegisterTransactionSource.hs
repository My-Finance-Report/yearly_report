{-# LANGUAGE OverloadedStrings #-}

module Routes.Crud.TransactionSource.RegisterTransactionSource (registerTransactionSourceRoutes) where

import Auth (requireUser)
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
import Database.Models (Category (Category), User (userOnboardingStep), parseSourceKind)
import Database.Persist hiding (get)
import Database.Persist.Postgresql (ConnectionPool, toSqlKey)
import Database.Transaction (getAllTransactions, groupTransactionsBySource, updateTransactionCategory)
import Database.TransactionSource (addTransactionSource, getAllTransactionSources, getTransactionSource, removeTransactionSource, updateTransactionSource)
import Database.UploadConfiguration (getAllUploadConfigs)
import HtmlGenerators.AuthPages (renderLoginPage)
import HtmlGenerators.ConfigurationNew (renderConfigurationPageNew)
import HtmlGenerators.HomePage (makeSimpleBanner, renderHomePage)
import HtmlGenerators.HtmlGenerators (renderSupportPage)
import HtmlGenerators.OnboardingOne (renderOnboardingOne)
import HtmlGenerators.OnboardingTwo (renderOnboardingTwo)
import Sankey (generateSankeyData)
import SankeyConfiguration (generateSankeyConfig)
import Types
import Web.Scotty (ActionM, ScottyM, formParam, formParams, get, header, html, pathParam, post, redirect, text)

registerTransactionSourceRoutes :: ConnectionPool -> ScottyM ()
registerTransactionSourceRoutes pool = do
  post "/add-transaction-source" $ requireUser pool $ \user -> do
    newSource <- Web.Scotty.formParam "newSource" :: Web.Scotty.ActionM Text
    kindText <- Web.Scotty.formParam "newKind" :: Web.Scotty.ActionM Text
    case parseSourceKind kindText of
      Right kind -> do
        _ <- liftIO $ addTransactionSource user newSource kind
        referer <- Web.Scotty.header "Referer"
        let redirectTo = fromMaybe "/dashboard" referer
        Web.Scotty.redirect redirectTo
      Left errMsg -> html $ fromStrict errMsg

  post "/remove-transaction-source" $ requireUser pool $ \user -> do
    sourceName <- Web.Scotty.formParam "sourceName" :: Web.Scotty.ActionM Text
    liftIO $ removeTransactionSource user sourceName

    referer <- Web.Scotty.header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer

    Web.Scotty.redirect redirectTo

  post "/edit-transaction-source/:id" $ requireUser pool $ \user -> do
    sourceIdText <- Web.Scotty.pathParam "id"
    let sourceId = toSqlKey $ read sourceIdText
    newSourceName <- Web.Scotty.formParam "updatedSourceName" :: Web.Scotty.ActionM Text
    liftIO $ updateTransactionSource user sourceId newSourceName

    referer <- Web.Scotty.header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer

    Web.Scotty.redirect redirectTo
