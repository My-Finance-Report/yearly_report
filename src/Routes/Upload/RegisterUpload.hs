{-# LANGUAGE OverloadedStrings #-}

module Routes.Upload.RegisterUpload (registerUploadRoutes) where

import Auth (getCurrentUser, requireUser)
import ColumnChart (generateColChartData)
import Control.Concurrent.Async (async)
import Control.Exception (SomeException (SomeException), try)
import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Data.HList (IORef, modifyIORef)
import Data.List (partition)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, isPrefixOf, pack, splitOn, unpack)
import qualified Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy
  ( fromStrict,
    intercalate,
    partition,
    toStrict,
  )
import qualified Data.Text.Lazy
import Database.Category (getCategoriesBySource, getCategory)
import Database.Configurations (getFirstSankeyConfig, saveSankeyConfig)
import Database.Database (updateUserOnboardingStep)
import Database.Files (addPdfRecord, getPdfRecord, getPdfRecords)
import Database.Models
import Database.Persist hiding (get)
import Database.Persist.Postgresql (ConnectionPool, fromSqlKey, toSqlKey)
import Database.Transaction (getAllTransactions, groupTransactionsBySource)
import Database.TransactionSource (getAllTransactionSources, getTransactionSource)
import Database.UploadConfiguration (addUploadConfigurationObject, getAllUploadConfigs, getUploadConfiguration, getUploadConfigurationFromPdf)
import ExampleFileParser (generateUploadConfiguration)
import HtmlGenerators.AuthPages (renderLoginPage)
import HtmlGenerators.Configuration (renderConfigurationPage)
import HtmlGenerators.ConfigurationNew (renderConfigurationPageNew)
import HtmlGenerators.HomePage (makeSimpleBanner, renderHomePage)
import HtmlGenerators.HtmlGenerators (renderSupportPage)
import HtmlGenerators.LandingPage (renderLandingPage)
import HtmlGenerators.Layout (renderPage)
import HtmlGenerators.UploadPage (renderSelectAccountPage, renderUploadPage)
import Network.Wai.Parse (FileInfo (..), tempFileBackEnd)
import Parsers (extractTextFromPdf, processPdfFile)
import Sankey (generateSankeyData)
import SankeyConfiguration (generateSankeyConfig)
import Types
import Web.Scotty (ActionM, ScottyM, files, formParam, formParams, get, header, html, json, pathParam, post, queryParam, redirect, setHeader, text)

processFileUpload user pdfId config activeJobs = do
  liftIO $ do
    modifyIORef activeJobs (+ 1)
    _ <- Control.Concurrent.Async.async $ do
      processPdfFile user pdfId config False
      modifyIORef activeJobs (subtract 1)
    return ()
  return ()

registerUploadRoutes :: ConnectionPool -> IORef Int -> ScottyM ()
registerUploadRoutes pool activeJobs = do
  get "/upload" $ requireUser pool $ \user -> do
    let content = renderUploadPage user
    html $ renderPage (Just user) "Upload Page" content

  post "/upload" $ requireUser pool $ \user -> do
    allFiles <- files
    let uploadedFiles = Prelude.filter (\(k, _) -> k == "pdfFiles") allFiles
    case uploadedFiles of
      [] -> text "No files were uploaded!"
      _ -> do
        fileConfigs <- forM uploadedFiles $ \(_, fileInfo) -> do
          let originalName = decodeUtf8 $ fileName fileInfo
          let tempFilePath = "/tmp/" <> originalName

          liftIO $ Data.ByteString.Lazy.writeFile (unpack tempFilePath) (fileContent fileInfo)

          extractedTextOrError <-
            liftIO $ try (extractTextFromPdf (unpack tempFilePath)) :: ActionM (Either SomeException Text)

          case extractedTextOrError of
            Left err -> return (fileInfo, Nothing, Nothing)
            Right extractedText -> do
              maybeConfig <- liftIO $ getUploadConfiguration user originalName extractedText
              return (fileInfo, maybeConfig, Just extractedText)

        pdfIds <- forM fileConfigs $ \(fileInfo, maybeConfig, maybeExtractedText) -> do
          let originalName = decodeUtf8 $ fileName fileInfo
          pdfId <- liftIO $ addPdfRecord user originalName (fromMaybe "" maybeExtractedText) "pending"
          return (pdfId, maybeConfig)

        let (missingConfigs, validConfigs) = Data.List.partition (isNothing . snd) pdfIds

        if null missingConfigs
          then do
            forM_ validConfigs $ \(pdfId, Just config) -> do
              processFileUpload user pdfId config activeJobs
            redirect "/dashboard"
          else do
            redirect $ "/select-account?pdfIds=" <> intercalate "," (map (Data.Text.Lazy.pack . show . fromSqlKey . fst) pdfIds)

  post "/assign-transaction-source" $ requireUser pool $ \user -> do
    params <- formParams

    let selections =
          [ (toSqlKey (read $ unpack (Data.Text.drop 7 key)) :: Key UploadedPdf, toSqlKey (read $ unpack val) :: Key TransactionSource)
            | (key, val) <- params,
              "source-" `isPrefixOf` key
          ]

    -- Process each selected transaction source
    forM_ selections $ \(pdfId, sourceId) -> do
      pdfRecord <- liftIO $ getPdfRecord user pdfId

      -- Ensure we have a valid file path to use for generating config
      let tempFilePath = "/tmp/" <> uploadedPdfFilename (entityVal pdfRecord)

      -- Generate and store the UploadConfiguration
      uploadConfig <- liftIO $ generateUploadConfiguration user sourceId tempFilePath
      case uploadConfig of
        Just config -> liftIO $ addUploadConfigurationObject user config
        Nothing -> liftIO $ putStrLn "Failed to generate UploadConfiguration."

    liftIO $ print "created the upload config"

    -- Reprocess all uploaded files
    forM_ selections $ \(pdfId, sourceId) -> do
      pdfRecord <- liftIO $ getPdfRecord user pdfId
      let extractedText = uploadedPdfRawContent (entityVal pdfRecord)
      maybeConfig <- liftIO $ getUploadConfigurationFromPdf user pdfId

      case maybeConfig of
        Just config -> processFileUpload user pdfId config activeJobs
        Nothing -> liftIO $ putStrLn "Failed to retrieve configuration for reprocessing."

    redirect "/dashboard"

  post "/upload-example-file/:sourceId" $ requireUser pool $ \user -> do
    liftIO $ print "***DEPRECATED, NO LONGER TO BE USED***"
    sourceIdText <- pathParam "sourceId" :: ActionM Text
    let sourceId = toSqlKey (read $ unpack sourceIdText) :: Key TransactionSource

    files <- files
    case lookup "exampleFile" files of
      Just fileInfo -> do
        let uploadedBytes = fileContent fileInfo
        let originalName = decodeUtf8 $ fileName fileInfo
        let tempFilePath = "/tmp/" <> originalName
        liftIO $ Data.ByteString.Lazy.writeFile (unpack tempFilePath) uploadedBytes

        liftIO $ do
          uploadConfig <- generateUploadConfiguration user sourceId tempFilePath
          case uploadConfig of
            Just config -> addUploadConfigurationObject user config
            Nothing -> putStrLn "Failed to generate UploadConfiguration from the example file."

        referer <- header "Referer"
        let redirectTo = fromMaybe "/dashboard" referer
        redirect redirectTo
      Nothing -> text "No file provided in the request"

  get "/select-account" $ requireUser pool $ \user -> do
    pdfIdsParam <- queryParam "pdfIds"

    let pdfIds = map (toSqlKey . read . unpack) pdfIdsParam :: [Key UploadedPdf]
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

    html $ renderPage (Just user) "Adjust Transactions" $ renderSelectAccountPage pdfsWithSources transactionSources
