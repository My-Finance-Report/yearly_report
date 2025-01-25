{-# LANGUAGE OverloadedStrings #-}

module Routes.Crud.File.RegisterFile (registerFileRoutes) where

import Auth (requireUser)
import ColumnChart (generateColChartData)
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.HList (IORef, modifyIORef)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, splitOn, unpack)
import Data.Text.Lazy (fromStrict, toStrict)
import Database.Category (addCategory, getCategoriesBySource, getCategory, removeCategory, updateCategory)
import Database.Configurations (getFirstSankeyConfig, saveSankeyConfig)
import Database.Database (updateUserOnboardingStep)
import Database.Files (deleteProcessedFile, getProcessedFile)
import Database.Models (Category (Category), ProcessedFile (processedFileUploadConfigurationId, processedFileUploadedPdfId), User (userOnboardingStep))
import Database.Persist hiding (get)
import Database.Persist.Postgresql (ConnectionPool, fromSqlKey, toSqlKey)
import Database.Transaction (getAllTransactions, groupTransactionsBySource, removeTransactionByPdfId, updateTransactionCategory)
import Database.TransactionSource (addTransactionSource, getAllTransactionSources, getTransactionSource, removeTransactionSource, updateTransactionSource)
import Database.UploadConfiguration (getAllUploadConfigs, getUploadConfigById)
import HtmlGenerators.AuthPages (renderLoginPage)
import HtmlGenerators.ConfigurationNew (renderConfigurationPageNew)
import HtmlGenerators.HomePage (makeSimpleBanner, renderHomePage)
import HtmlGenerators.HtmlGenerators (renderSupportPage)
import HtmlGenerators.OnboardingOne (renderOnboardingOne)
import HtmlGenerators.OnboardingTwo (renderOnboardingTwo)
import Parsers (processPdfFile)
import Sankey (generateSankeyData)
import SankeyConfiguration (generateSankeyConfig)
import Web.Scotty (ActionM, ScottyM, formParam, formParams, get, header, html, json, pathParam, post, redirect, text)

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
    (Nothing, Nothing) -> text "Error: Processed file does not have an associated uploaded PDF or config"
    (Nothing, _) -> text "Error: Processed file does not have an associated uploaded PDF."
    (_, Nothing) -> text "Error: Processed file does not have an associated upload configuration."
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

      text "Reprocessing started successfully!"

registerFileRoutes :: ConnectionPool -> IORef Int -> ScottyM ()
registerFileRoutes pool activeJobs = do
  post "/reprocess-file/:fId" $ requireUser pool $ \user -> do
    fIdText <- Web.Scotty.pathParam "fId"

    let processedFileId = toSqlKey $ read fIdText

    liftIO $ print "reprocessing"
    reprocessFileUpload user processedFileId activeJobs

    Web.Scotty.redirect "/dashboard"
