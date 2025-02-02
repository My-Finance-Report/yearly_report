{-# LANGUAGE OverloadedStrings #-}

module Database.Files
  ( getAllFilenames,
    getPdfRecord,
    getPdfRecords,
    addPdfRecord,
    isFileProcessed,
    getAllProcessedFiles,
  )
where

import Control.Exception (throwIO)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.List (nubBy, sortOn)
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

getPdfRecords :: (MonadUnliftIO m) => Entity User -> [Key UploadedPdf] -> m [Entity UploadedPdf]
getPdfRecords user pdfIds = do
  pool <- liftIO getConnectionPool
  runSqlPool queryPdfRecords pool
  where
    queryPdfRecords =
      selectList
        [UploadedPdfId <-. pdfIds, UploadedPdfUserId ==. entityKey user]
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

isFileProcessed :: (MonadUnliftIO m) => Entity User -> Entity UploadedPdf -> m Bool
isFileProcessed user file = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool query pool
  return $ isJust result
  where
    query =
      selectFirst
        [ ProcessFileJobPdfId ==. entityKey file,
          ProcessFileJobStatus ==. Completed
        ]
        []

getAllProcessedFiles :: (MonadUnliftIO m) => Entity User -> m [(Entity ProcessFileJob, Entity UploadedPdf)]
getAllProcessedFiles user = do
  pool <- liftIO getConnectionPool

  jobs <- runSqlPool (selectList [ProcessFileJobUserId ==. entityKey user] []) pool
  
  let pdfIds = map (processFileJobPdfId . entityVal) jobs

  pdfs <- runSqlPool (selectList [UploadedPdfId <-. pdfIds] [Asc UploadedPdfFilename]) pool

  let pdfMap = Data.Map.fromList [(entityKey pdf, pdf) | pdf <- pdfs]

  let results = [(job, pdf) | job <- jobs, Just pdf <- [Data.Map.lookup (processFileJobPdfId (entityVal job)) pdfMap]]

  let sortedResults = Data.List.sortOn (uploadedPdfFilename . entityVal . snd) results

  return sortedResults