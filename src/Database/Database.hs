{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Database
  ( seedDatabase,
    getAllFilenames,
    fetchPdfRecord,
    isFileProcessed,
    markFileAsProcessed,
    insertPdfRecord,
    getSourceFileMappings,
    fetchSourceMap,
  )
where

import ConnectionPool
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (unliftIO))
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.Map hiding (insert, update)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Category (ensureCategoriesExist, ensureCategoryExists)
import Database.Persist (Entity (..), Filter)
import Database.Persist.Postgresql (insert, rawSql, runSqlPool, selectFirst, (==.))
import Database.Persist.Sql
import Database.TransactionSource (ensureTransactionSourceExists)
import Models
import Types

seedDatabase :: Entity User -> IO ()
seedDatabase user = do
  pool <- getConnectionPool
  runSqlPool
    ( do
        -- Create or get transaction sources
        bankSourceId <- ensureTransactionSourceExists user "Bank"
        ccSourceId <- ensureTransactionSourceExists user "CreditCard"

        -- Create categories for the bank source
        ensureCategoriesExist
          user
          bankSourceId
          ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]

        -- Create categories for the credit card source
        ensureCategoriesExist
          user
          ccSourceId
          ["Groceries", "Travel", "Gas", "Misc", "Subscriptions", "Food"]

        liftIO $ putStrLn "Database seeded successfully!"
    )
    pool



getAllFilenames :: (MonadUnliftIO m) => m [Text]
getAllFilenames = do
  pool <- liftIO getConnectionPool
  runSqlPool queryFilenames pool
  where
    queryFilenames = do
      results <- selectList [] [Asc UploadedPdfId]
      return $ Prelude.map (uploadedPdfFilename . entityVal) results


  

fetchPdfRecord :: (MonadUnliftIO m) => Key UploadedPdf -> m UploadedPdf
fetchPdfRecord pdfId = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool (get pdfId) pool
  case result of
    Just pdf -> return pdf
    Nothing -> liftIO $ error $ "No PDF found with id=" ++ show (fromSqlKey pdfId)

insertPdfRecord :: (MonadUnliftIO m) => T.Text -> T.Text -> T.Text -> m (Key UploadedPdf)
insertPdfRecord filename rawContent uploadTime = do
  pool <- liftIO getConnectionPool
  runSqlPool queryInsertPdfRecord pool
  where
    queryInsertPdfRecord = do
      insert $
        UploadedPdf
          { uploadedPdfFilename = filename,
            uploadedPdfRawContent = rawContent,
            uploadedPdfUploadTime = uploadTime
          }

isFileProcessed :: (MonadUnliftIO m) => Text -> m Bool
isFileProcessed filename = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool (selectFirst [ProcessedFileFilename ==. filename] []) pool
  return $ isJust result

markFileAsProcessed :: (MonadUnliftIO m) => Text -> m ()
markFileAsProcessed filename = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool (insertUnique $ ProcessedFile filename) pool
  case result of
    Just _ -> liftIO $ putStrLn $ "File processed: " <> T.unpack filename
    Nothing -> liftIO $ putStrLn $ "File already marked as processed: " <> T.unpack filename




getSourceFileMappings :: (MonadUnliftIO m) => m [SourceFileMapping]
getSourceFileMappings = do
  pool <- liftIO getConnectionPool
  runSqlPool querySourceFileMappings pool
  where
    querySourceFileMappings = do
      -- Fetch all transaction sources, transactions, and uploaded PDFs
      sources <- selectList [] []
      transactions <- selectList [] []
      pdfs <- selectList [] []

      -- Convert PDFs to a Map for efficient lookups
      let pdfMap = Map.fromList [(entityKey pdf, entityVal pdf) | pdf <- pdfs]

      -- Group transactions by source with unique filenames
      let sourceToFiles =
            Map.fromListWith
              Set.union
              [ (transactionTransactionSourceId txn, Set.singleton (uploadedPdfFilename pdf))
                | Entity _ txn <- transactions,
                  Just pdfId <- [transactionUploadedPdfId txn],
                  Just pdf <- [Map.lookup pdfId pdfMap]
              ]

      -- Convert the Set of filenames back to a list for `handledFiles`
      return
        [ SourceFileMapping
            { source = source,
              handledFiles = Set.toList $ Map.findWithDefault Set.empty (entityKey source) sourceToFiles
            }
          | source <- sources
        ]

fetchSourceMap :: (MonadUnliftIO m) => m (Map (Key TransactionSource) TransactionSource)
fetchSourceMap = do
  pool <- liftIO getConnectionPool
  runSqlPool querySourceMap pool
  where
    querySourceMap = do
      sources <- selectList [] []
      return $ Map.fromList [(entityKey source, entityVal source) | source <- sources]
