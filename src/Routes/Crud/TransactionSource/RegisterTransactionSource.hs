
{-# LANGUAGE OverloadedStrings #-}
module Routes.Crud.TransactionSource.RegisterTransactionSource(registerTransactionSourceRoutes) where



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
import Database.Persist hiding (get)
import Database.Persist.Postgresql (ConnectionPool, toSqlKey)
import Database.Transaction (getAllTransactions, groupTransactionsBySource, updateTransactionCategory)
import Database.TransactionSource (getAllTransactionSources, getTransactionSource, updateTransactionSource, addTransactionSource, removeTransactionSource)
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
import HtmlGenerators.OnboardingTwo (renderOnboardingTwo)
import HtmlGenerators.OnboardingOne (renderOnboardingOne)
import Auth (requireUser)

registerTransactionSourceRoutes :: ConnectionPool -> ScottyM ()
registerTransactionSourceRoutes pool = do
    post "/add-transaction-source" $ requireUser pool $ \user -> do
      newSource <- Web.Scotty.formParam "newSource" :: Web.Scotty.ActionM Text
      liftIO $ addTransactionSource user newSource

      referer <- Web.Scotty.header "Referer"
      let redirectTo = fromMaybe "/dashboard" referer

      Web.Scotty.redirect redirectTo

    post "/remove-transaction-source" $ requireUser pool $ \user -> do
      newSource <- Web.Scotty.formParam "newSource" :: Web.Scotty.ActionM Text
      liftIO $ removeTransactionSource user newSource

      referer <- Web.Scotty.header "Referer"
      let redirectTo = fromMaybe "/dashboard" referer

      Web.Scotty.redirect redirectTo

    post "/edit-transaction-source/:id" $ requireUser pool $ \user -> do
      sourceIdText <- Web.Scotty.pathParam "id"
      let sourceId = toSqlKey $ read sourceIdText
      sourceName <- Web.Scotty.formParam "sourceName" :: Web.Scotty.ActionM Text
      liftIO $ updateTransactionSource user sourceId sourceName
      Web.Scotty.redirect "/configuration"



    get "/add-account/step-1" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      let content = renderOnboardingOne user transactionSources False
      Web.Scotty.html $ renderPage (Just user) "Add Account" content

    get "/add-account/step-2" $ requireUser pool $ \user -> do
      transactionSources <- liftIO $ getAllTransactionSources user
      categoriesBySource <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip transactionSources categories

      let content = renderOnboardingTwo user categoriesBySource False
      Web.Scotty.html $ renderPage (Just user) "Add Account" content





