{-# LANGUAGE OverloadedStrings #-}

module Database.Files
  ( getAllFilenames,
    getPdfRecord,
    addPdfRecord,
    isFileProcessed,
    markFileAsProcessed,
    getSourceFileMappings,
  )
where

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

getPdfRecord :: (MonadUnliftIO m) => Entity User -> Key UploadedPdf -> m UploadedPdf
getPdfRecord user pdfId = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool queryPdfRecord pool
  case result of
    Just (Entity _ pdf) -> return pdf
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

markFileAsProcessed :: (MonadUnliftIO m) => Entity User -> Text -> m ()
markFileAsProcessed user filename = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool (insertUnique $ ProcessedFile filename (entityKey user)) pool
  case result of
    Just _ -> liftIO $ putStrLn $ "File processed: " <> unpack filename
    Nothing -> liftIO $ putStrLn $ "File already marked as processed: " <> unpack filename

getSourceFileMappings :: (MonadUnliftIO m) => Entity User -> m [SourceFileMapping]
getSourceFileMappings user = do
  pool <- liftIO getConnectionPool
  runSqlPool querySourceFileMappings pool
  where
    querySourceFileMappings = do
      -- Get all transaction sources for the user
      sources <- getAllTransactionSources user

      -- Get all transactions for the user
      transactions <- selectList [TransactionUserId ==. entityKey user] []

      -- Get all PDFs belonging to the user
      pdfs <- selectList [UploadedPdfUserId ==. entityKey user] []

      -- Create a map of PDF IDs to their respective records
      let pdfMap = fromList [(entityKey pdf, entityVal pdf) | pdf <- pdfs]

      -- Map transaction sources to handled files
      let sourceToFiles =
            fromListWith
              union
              [ (transactionTransactionSourceId txn, singleton (uploadedPdfFilename pdf))
                | Entity _ txn <- transactions,
                  Just pdfId <- [transactionUploadedPdfId txn],
                  Just pdf <- [Data.Map.lookup pdfId pdfMap]
              ]

      -- Create SourceFileMapping for each source
      return
        [ SourceFileMapping
            { source = source,
              handledFiles = toList $ findWithDefault empty (entityKey source) sourceToFiles
            }
          | source <- sources
        ]
