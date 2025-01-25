{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Auth
import Categorizer
import ColumnChart
import Control.Concurrent.Async (async)
import Control.Exception (SomeException (SomeException), try)
import Control.Monad (forM, forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson hiding (Key)
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.List (foldl', nub, partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime (..), defaultTimeLocale, formatTime, fromGregorian, parseTimeM, toGregorian)
import Database.Category
  ( addCategory,
    getCategoriesBySource,
    getCategory,
    removeCategory,
    updateCategory,
  )
import Database.Configurations
import Database.ConnectionPool
import Database.Files
import Database.Models
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
import Database.Persist.Postgresql hiding (get)
import Database.Transaction
import Database.TransactionSource
import Database.UploadConfiguration
import ExampleFileParser
import GHC.Generics (Generic)
import HtmlGenerators.AllFilesPage
import HtmlGenerators.AuthPages (renderLoginPage)
import HtmlGenerators.Configuration (renderConfigurationPage)
import HtmlGenerators.ConfigurationNew (renderConfigurationPageNew)
import HtmlGenerators.HomePage
import HtmlGenerators.HtmlGenerators
import HtmlGenerators.LandingPage
import HtmlGenerators.Layout (renderPage)
import HtmlGenerators.OnboardingFour
import HtmlGenerators.OnboardingOne
import HtmlGenerators.OnboardingThree
import HtmlGenerators.OnboardingTwo
import HtmlGenerators.UploadPage
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Network.Wai.Parse (FileInfo (..), tempFileBackEnd)
import Parsers
import Routes.Api.Visualization.RegisterVisualization (registerVisualizationRoutes)
import Routes.Configuration.RegisterConfiguration (registerConfigurationRoutes)
import Routes.Crud.Category.RegisterCategory (registerCategoryRoutes)
import Routes.Crud.Sankey.RegisterSankey (registerSankeyRoutes)
import Routes.Demo.RegisterDemo (registerDemoRoutes)
import Routes.Login.RegisterLogin (registerLoginRoutes)
import Routes.Misc.RegisterMisc (registerMiscRoutes)
import Routes.Onboarding.RegisterOnboarding
  ( registerOnboardingRoutes,
  )
import Routes.Upload.RegisterUpload (registerUploadRoutes)
import Sankey
import SankeyConfiguration
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)
import Types
import Web.Scotty
  ( ActionM,
    File,
    files,
    finish,
    formParam,
    formParams,
    get,
    header,
    html,
    json,
    liftIO,
    middleware,
    pathParam,
    post,
    redirect,
    scotty,
    text,
  )
import qualified Web.Scotty as Web

parseDate :: Text -> Maybe UTCTime
parseDate dateText = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateText)

getRequiredEnv :: String -> IO String
getRequiredEnv key = do
  maybeValue <- lookupEnv key
  case maybeValue of
    Just value -> return value
    Nothing -> ioError $ userError $ "Environment variable not set: " ++ key

deleteFileAndTransactions ::
  Entity User ->
  Key ProcessedFile ->
  IORef Int ->
  Web.Scotty.ActionM ()
deleteFileAndTransactions user processedFileId activeJobs = do
  processedFile <- liftIO $ getProcessedFile user processedFileId
  let fileId = entityKey processedFile
  liftIO $ putStrLn "Deleting with a valid config"
  let pdfId = processedFileUploadedPdfId $ entityVal processedFile
  case pdfId of
    Nothing -> redirect "/dashboard"
    Just validPdfId -> do
      liftIO $ removeTransactionByPdfId user validPdfId
      liftIO $ deleteProcessedFile user fileId

reprocessFileUpload ::
  Entity User ->
  Key ProcessedFile ->
  IORef Int ->
  Web.Scotty.ActionM ()
reprocessFileUpload user processedFileId activeJobs = do
  processedFile <- liftIO $ getProcessedFile user processedFileId
  let pdfId = processedFileUploadedPdfId $ entityVal processedFile
  let uploadConfigId = processedFileUploadConfigurationId $ entityVal processedFile

  liftIO $ print (show pdfId <> show uploadConfigId)

  case (pdfId, uploadConfigId) of
    (Nothing, Nothing) -> Web.text "Error: Processed file does not have an associated uploaded PDF or config"
    (Nothing, _) -> Web.text "Error: Processed file does not have an associated uploaded PDF."
    (_, Nothing) -> Web.text "Error: Processed file does not have an associated upload configuration."
    (Just validPdfId, Just validUploadConfigId) -> do
      liftIO $ putStrLn "Reprocessing with a valid config"
      liftIO $ do
        putStrLn $ "Reprocessing PDF ID: " <> show (fromSqlKey validPdfId)
        putStrLn $ "Using upload config ID: " <> show (fromSqlKey validUploadConfigId)

        modifyIORef activeJobs (+ 1)
        _ <- Control.Concurrent.Async.async $ do
          uploadConfig <- getUploadConfigById user validUploadConfigId
          removeTransactionByPdfId user validPdfId
          processPdfFile user validPdfId uploadConfig True
          modifyIORef activeJobs (subtract 1)
          putStrLn $ "Finished reprocessing PDF ID: " <> show (fromSqlKey validPdfId)
          return ()
        return ()

      -- Ensure response is sent after the async job starts
      Web.text "Reprocessing started successfully!"

main :: IO ()
main = do
  activeJobs <- newIORef 0
  openAiKey <- liftIO $ getRequiredEnv "OPENAI_API_KEY"
  openAiKey <- liftIO $ getRequiredEnv "DATABASE_URL"
  initializePool
  pool <- getConnectionPool
  migratePostgres
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static")

    registerOnboardingRoutes pool
    registerLoginRoutes pool
    registerMiscRoutes pool activeJobs
    registerVisualizationRoutes pool
    registerDemoRoutes pool
    registerSankeyRoutes pool
    registerConfigurationRoutes pool
    registerUploadRoutes pool activeJobs
    registerCategoryRoutes pool

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

    get "/manage-accounts" $ requireUser pool $ \user -> do
      Web.Scotty.html $ renderPage (Just user) "Manage Accounts" "this page is not ready yet :) "

    get "/select-account" $ requireUser pool $ \user -> do
      pdfIdsParam <- Web.queryParam "pdfIds"

      -- Fetch pending PDF records from the database
      let pdfIds = map (toSqlKey . read . T.unpack) pdfIdsParam :: [Key UploadedPdf]
      fileRecords <- liftIO $ getPdfRecords user pdfIds

      liftIO $ print pdfIds

      transactionSources <- liftIO $ getAllTransactionSources user

      let txnLookup = Map.fromList [(entityKey txn, txn) | txn <- transactionSources]

      liftIO $ print txnLookup

      pdfsWithSources <- forM fileRecords $ \record -> do
        maybeConfig <- liftIO $ getUploadConfigurationFromPdf user (entityKey record)
        let maybeTxnSource = maybeConfig >>= \(Entity _ config) -> Map.lookup (uploadConfigurationTransactionSourceId config) txnLookup
        return (record, maybeTxnSource)

      transactionSources <- liftIO $ getAllTransactionSources user

      Web.html $ renderPage (Just user) "Adjust Transactions" $ renderSelectAccountPage pdfsWithSources transactionSources

    get "/transactions" $ requireUser pool $ \user -> do
      filenames <- liftIO $ getAllFilenames user
      Web.html $ renderPage (Just user) "Adjust Transactions" $ renderAllFilesPage filenames

    get "/transactions/:fileid" $ requireUser pool $ \user -> do
      fileIdText <- Web.Scotty.pathParam "fileid"

      let fileId = toSqlKey (read $ T.unpack fileIdText) :: Key UploadedPdf
      uploadedFile <- getPdfRecord user fileId
      transactions <- liftIO $ getTransactionsByFileId user fileId
      transactionSources <- liftIO $ getAllTransactionSources user
      categoryLookup <- liftIO $ do
        categories <- Prelude.mapM (getCategoriesBySource user . entityKey) transactionSources
        return $ Map.fromList $ zip (Prelude.map entityKey transactionSources) categories

      Web.html $ renderPage (Just user) "Adjust Transactions" $ renderTransactionsPage uploadedFile categoryLookup transactions

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

    post "/update-transaction/:id" $ requireUser pool $ \user -> do
      txIdText <- Web.Scotty.pathParam "id" :: Web.Scotty.ActionM Int
      fileIdText <- Web.Scotty.formParam "fileId" :: Web.Scotty.ActionM Text
      let txId = toSqlKey (fromIntegral txIdText)

      mDescription <- Web.Scotty.formParam "description" >>= \d -> return $ readMaybe d
      mDate <- Web.Scotty.formParam "transactionDate" >>= \d -> return $ parseDate d
      mAmount <- Web.Scotty.formParam "amount" >>= \a -> return $ readMaybe a
      mCategoryId <- Web.Scotty.formParam "category" >>= \c -> return $ Just (toSqlKey $ read c)
      mKind <-
        Web.Scotty.formParam "kind" >>= \k -> return $ case (k :: Text) of
          "Withdrawal" -> Just Withdrawal
          "Deposit" -> Just Deposit
          _ -> Nothing

      liftIO $ updateTransaction user txId mDescription mDate mAmount mKind mCategoryId

      let redirectUrl = TL.fromStrict $ T.append "/transactions/" fileIdText
      Web.Scotty.redirect redirectUrl

    post "/remove-transaction/:tId" $ requireUser pool $ \user -> do
      tIdText <- Web.Scotty.pathParam "tId"
      let tId = toSqlKey $ read tIdText
      liftIO $ removeTransaction user tId

      referer <- Web.Scotty.header "Referer"
      let redirectTo = fromMaybe "/dashboard" referer

      Web.Scotty.redirect redirectTo

    post "/reprocess-file/:fId" $ requireUser pool $ \user -> do
      fIdText <- Web.Scotty.pathParam "fId"

      let processedFileId = toSqlKey $ read fIdText

      liftIO $ print "reprocessing"
      reprocessFileUpload user processedFileId activeJobs

      Web.Scotty.redirect "/dashboard"
