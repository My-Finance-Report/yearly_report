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
import Routes.Crud.Transaction.RegisterTransaction (registerTransactionRoutes)
import Routes.Crud.TransactionSource.RegisterTransactionSource (registerTransactionSourceRoutes)
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
    registerTransactionRoutes pool
    registerTransactionSourceRoutes pool


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


    post "/reprocess-file/:fId" $ requireUser pool $ \user -> do
      fIdText <- Web.Scotty.pathParam "fId"

      let processedFileId = toSqlKey $ read fIdText

      liftIO $ print "reprocessing"
      reprocessFileUpload user processedFileId activeJobs

      Web.Scotty.redirect "/dashboard"
