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
import Data.Time (UTCTime)
import Database.Category (getCategoriesBySource, getCategory)
import Database.Configurations (getFirstSankeyConfig, saveSankeyConfig)
import Database.ConnectionPool (getConnectionPool)
import Database.Database (updateUserOnboardingStep)
import Database.Files (addPdfRecord, computeMD5, getPdfRecord, getPdfRecordByHash, getPdfRecords)
import Database.Models
import Database.Persist hiding (get)
import Database.Persist.Postgresql (ConnectionPool, SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import Database.Transaction (getAllTransactions, groupTransactionsBySource)
import Database.TransactionSource (getAllTransactionSources, getTransactionSource)
import Database.UploadConfiguration (addUploadConfigurationObject, getAllUploadConfigs, getUploadConfiguration, getUploadConfigurationFromPdf)
import ExampleFileParser (generateUploadConfiguration)
import HtmlGenerators.Layout (renderPage)
import HtmlGenerators.UploadPage (renderSelectAccountPage, renderUploadPage)
import Network.Wai.Parse (FileInfo (..), defaultParseRequestBodyOptions, lbsBackEnd, parseRequestBodyEx, setMaxRequestNumFiles, tempFileBackEnd)
import Parsers (extractTextFromPdf)
import Sankey (generateSankeyData)
import Types
import Web.Scotty (ActionM, ScottyM, files, formParam, formParams, get, header, html, json, pathParam, post, queryParam, redirect, request, setHeader, text)
import Worker.ParseFileJob

registerUploadRoutes :: ConnectionPool -> ScottyM ()
registerUploadRoutes pool = do
  get "/upload" $ requireUser pool $ \user -> do
    content <- liftIO $ renderUploadPage user
    html $ renderPage (Just user) "Upload Page" content True

  post "/upload" $ requireUser pool $ \user -> do
    req <- request

    let options = setMaxRequestNumFiles 50 defaultParseRequestBodyOptions

    (params, uploadedFiles) <- liftIO $ parseRequestBodyEx options lbsBackEnd req

    let filteredFiles = Prelude.filter (\(k, _) -> k == "pdfFiles") uploadedFiles

    case filteredFiles of
      [] -> text "No files were uploaded!"
      _ -> do
        fileConfigs <- forM uploadedFiles $ \(_, fileInfo) -> do
          let originalName = decodeUtf8 $ fileName fileInfo
          let tempFilePath = "/tmp/" <> originalName

          liftIO $ Data.ByteString.Lazy.writeFile (unpack tempFilePath) (fileContent fileInfo)

          extractedTextOrError <-
            liftIO $ try (extractTextFromPdf (unpack tempFilePath)) :: ActionM (Either SomeException Text)

          case extractedTextOrError of
            Left err -> return (fileInfo, Nothing, Nothing, Nothing)
            Right extractedText -> do
              maybeUploadedPdf <- liftIO $ getPdfRecordByHash user (computeMD5 extractedText)
              maybeConfig <- liftIO $ getUploadConfiguration user originalName extractedText

              return (fileInfo, maybeConfig, maybeUploadedPdf, Just extractedText)

        pdfIds <- forM fileConfigs $ \(fileInfo, maybeConfig, maybeUploadedPdf, maybeExtractedText) -> do
          case maybeUploadedPdf of
            Nothing -> do
              let originalName = decodeUtf8 $ fileName fileInfo
              pdfId <- liftIO $ addPdfRecord user originalName (fromMaybe "" maybeExtractedText) "todo"
              return (pdfId, maybeConfig)
            Just exisitingPdf -> return (entityKey exisitingPdf, maybeConfig)

        forM_ pdfIds $ \(pdfId, config) -> liftIO $ asyncFileProcess user pdfId (fmap entityKey config)
        redirect "/dashboard"

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

    html $ renderPage (Just user) "Adjust Transactions" (renderSelectAccountPage pdfsWithSources transactionSources) True
