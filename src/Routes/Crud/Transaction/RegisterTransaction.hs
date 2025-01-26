{-# LANGUAGE OverloadedStrings #-}

module Routes.Crud.Transaction.RegisterTransaction (registerTransactionRoutes) where

import Auth (requireUser)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, append, splitOn, unpack)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Database.Category (addCategory, getCategoriesBySource, getCategory, removeCategory, updateCategory)
import Database.Configurations (getFirstSankeyConfig, saveSankeyConfig)
import Database.Database (updateUserOnboardingStep)
import Database.Files (getAllFilenames, getPdfRecord)
import Database.Models (Category (Category), UploadedPdf (UploadedPdf), User (userOnboardingStep))
import Database.Persist hiding (get)
import Database.Persist.Postgresql (ConnectionPool, toSqlKey)
import Database.Transaction (getAllTransactions, getTransactionsByFileId, groupTransactionsBySource, removeTransaction, updateTransaction, updateTransactionCategory)
import Database.TransactionSource (getAllTransactionSources, getTransactionSource)
import Database.UploadConfiguration (getAllUploadConfigs)
import HtmlGenerators.AllFilesPage (renderAllFilesPage)
import HtmlGenerators.ConfigurationNew (renderConfigurationPageNew)
import HtmlGenerators.HomePage (makeSimpleBanner, renderHomePage)
import HtmlGenerators.HtmlGenerators (renderSupportPage, renderTransactionsPage)
import HtmlGenerators.Layout (renderPage)
import Text.Read (readMaybe)
import Types
import Web.Scotty (ActionM, ScottyM, formParam, formParams, get, header, html, json, pathParam, post, redirect)

parseDate :: Text -> Maybe UTCTime
parseDate dateText = parseTimeM True defaultTimeLocale "%Y-%m-%d" (unpack dateText)

registerTransactionRoutes :: ConnectionPool -> ScottyM ()
registerTransactionRoutes pool = do
  post "/remove-transaction/:tId" $ requireUser pool $ \user -> do
    tIdText <- pathParam "tId"
    let tId = toSqlKey $ read tIdText
    liftIO $ removeTransaction user tId

    referer <- header "Referer"
    let redirectTo = fromMaybe "/dashboard" referer

    redirect redirectTo

  post "/update-transaction/:id" $ requireUser pool $ \user -> do
    txIdText <- pathParam "id" :: Web.Scotty.ActionM Int
    fileIdText <- formParam "fileId" :: Web.Scotty.ActionM Text
    let txId = toSqlKey (fromIntegral txIdText)

    mDescription <- formParam "description" >>= \d -> return $ readMaybe d
    mDate <- formParam "transactionDate" >>= \d -> return $ parseDate d
    mAmount <- formParam "amount" >>= \a -> return $ readMaybe a
    mCategoryId <- formParam "category" >>= \c -> return $ Just (toSqlKey $ read c)
    mKind <-
      formParam "kind" >>= \k -> return $ case (k :: Text) of
        "Withdrawal" -> Just Withdrawal
        "Deposit" -> Just Deposit
        _ -> Nothing

    liftIO $ updateTransaction user txId mDescription mDate mAmount mKind mCategoryId

    let redirectUrl = fromStrict $ append "/transactions/" fileIdText
    redirect redirectUrl

  get "/transactions" $ requireUser pool $ \user -> do
    filenames <- liftIO $ getAllFilenames user
    let content = renderAllFilesPage filenames
    html $ renderPage (Just user) "Adjust Transactions" content True

  get "/transactions/:fileid" $ requireUser pool $ \user -> do
    fileIdText <- pathParam "fileid"

    let fileId = toSqlKey (read $ unpack fileIdText) :: Key UploadedPdf
    uploadedFile <- getPdfRecord user fileId
    transactions <- liftIO $ getTransactionsByFileId user fileId
    transactionSources <- liftIO $ getAllTransactionSources user
    categoryLookup <- liftIO $ do
      categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
      return $ Map.fromList $ zip (Prelude.map entityKey transactionSources) categories
    let content = renderTransactionsPage uploadedFile categoryLookup transactions
    html $ renderPage (Just user) "Adjust Transactions" content True
