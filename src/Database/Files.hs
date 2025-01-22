{-# LANGUAGE OverloadedStrings #-}

module Database.Files
  ( getAllFilenames,
    getPdfRecord,
    addPdfRecord,
    isFileProcessed,
    markFileAsProcessed,
    getAllProcessedFiles,
    getProcessedFile,
  )
where

import Control.Exception (throwIO)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.List (nubBy)
import Data.Map (Map, findWithDefault, fromList, fromListWith, lookup, mapKeys)
import Data.Maybe (isJust)
import Data.Set (Set, empty, singleton, toList, union)
import Data.Text (Text, unpack)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Database.Category
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist (Entity (..), PersistEntity (Key), SelectOpt (Asc))
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)
import Database.Transaction
import Database.TransactionSource
import Types

getAllFilenames :: (MonadUnliftIO m) => Entity User -> m [Entity UploadedPdf]
getAllFilenames user = do
  pool <- liftIO getConnectionPool
  runSqlPool queryFilenames pool
  where
    queryFilenames = do
      results <- selectList [UploadedPdfUserId ==. entityKey user] [Asc UploadedPdfId]
      return $ nubBy (\a b -> uploadedPdfFilename (entityVal a) == uploadedPdfFilename (entityVal b)) results

getPdfRecord :: (MonadUnliftIO m) => Entity User -> Key UploadedPdf -> m (Entity UploadedPdf)
getPdfRecord user pdfId = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool queryPdfRecord pool
  case result of
    Just pdf -> return pdf
    Nothing -> liftIO $ error $ "No PDF found with id=" ++ show (fromSqlKey pdfId) ++ " for user id=" ++ show (fromSqlKey $ entityKey user)
  where
    queryPdfRecord =
      selectFirst
        [UploadedPdfId ==. pdfId, UploadedPdfUserId ==. entityKey user]
        []

addPdfRecord :: (MonadUnliftIO m) => Entity User -> Text -> Text -> Text -> m (Key UploadedPdf)
addPdfRecord user filename rawContent uploadTime = do
  pool <- liftIO getConnectionPool
  runSqlPool queryInsertPdfRecord pool
  where
    queryInsertPdfRecord = do
      insert $
        UploadedPdf
          { uploadedPdfFilename = filename,
            uploadedPdfRawContent = rawContent,
            uploadedPdfUploadTime = uploadTime,
            uploadedPdfUserId = entityKey user
          }

isFileProcessed :: (MonadUnliftIO m) => Entity User -> Text -> m Bool
isFileProcessed user filename = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool (selectFirst [ProcessedFileFilename ==. filename, ProcessedFileUserId ==. entityKey user] []) pool
  return $ isJust result

getProcessedFile :: (MonadUnliftIO m) => Entity User -> Key ProcessedFile -> m (Entity ProcessedFile)
getProcessedFile user fileId = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool queryProcessedFile pool
  liftIO $ maybe (throwIO $ userError "Processed file not found") pure result
  where
    queryProcessedFile = selectFirst [ProcessedFileId ==. fileId] []

markFileAsProcessed :: (MonadUnliftIO m) => Entity User -> Text -> Maybe (Key UploadConfiguration) -> Maybe (Key UploadedPdf) -> m ()
markFileAsProcessed user filename uploadConfigId uploadedFileId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryInsertOrUpdate pool
  where
    queryInsertOrUpdate = do
      maybeExistingFile <- getBy $ UniqueProcessedFile filename (entityKey user)
      case maybeExistingFile of
        Nothing -> do
          _ <- insert $ ProcessedFile filename (entityKey user) uploadConfigId uploadedFileId
          liftIO $ putStrLn $ "File processed: " <> unpack filename
        Just (Entity fileId _) -> do
          update
            fileId
            [ ProcessedFileUploadConfigurationId =. uploadConfigId,
              ProcessedFileUploadedPdfId =. uploadedFileId
            ]
          liftIO $ putStrLn $ "File updated: " <> unpack filename

getAllProcessedFiles :: (MonadUnliftIO m) => Entity User -> m [Entity ProcessedFile]
getAllProcessedFiles user = do
  pool <- liftIO getConnectionPool
  runSqlPool (selectList [ProcessedFileUserId ==. entityKey user] [Asc ProcessedFileFilename]) pool
