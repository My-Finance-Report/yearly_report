{-# LANGUAGE OverloadedStrings #-}

module Database.Files
  ( getAllFilenames,
    getPdfRecord,
    getPdfRecordByHash,
    getPdfRecords,
    addPdfRecord,
    isFileProcessed,
    getAllProcessedFiles,
    computeMD5,
  )
where

import Control.Exception (throwIO)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Crypto.Hash (Digest, MD5, hash)
import qualified Data.ByteString as B16
import Data.List (nubBy, sortOn)
import Data.Map (Map, findWithDefault, fromList, fromListWith, lookup, mapKeys)
import Data.Maybe (catMaybes, isJust, isNothing, mapMaybe)
import Data.Set (Set, empty, singleton, toList, union)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as B16
import qualified Data.Text.Encoding as TE
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

getPdfRecordByHash :: (MonadUnliftIO m) => Entity User -> Text -> m (Maybe (Entity UploadedPdf))
getPdfRecordByHash user hash = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool queryPdfRecord pool
  case result of
    Just pdf -> return (Just pdf)
    Nothing -> return Nothing
  where
    queryPdfRecord =
      selectFirst
        [UploadedPdfRawContentHash ==. hash, UploadedPdfUserId ==. entityKey user]
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
  let rawContentHash = computeMD5 rawContent
  pool <- liftIO getConnectionPool
  runSqlPool (queryInsertPdfRecord rawContentHash) pool
  where
    queryInsertPdfRecord hash = do
      insert $
        UploadedPdf
          { uploadedPdfFilename = filename,
            uploadedPdfRawContent = rawContent,
            uploadedPdfRawContentHash = hash,
            uploadedPdfArchived = False,
            uploadedPdfUploadTime = uploadTime,
            uploadedPdfUserId = entityKey user
          }

computeMD5 :: Text -> Text
computeMD5 txt =
  let digest :: Digest MD5
      digest = hash (TE.encodeUtf8 txt)
   in T.pack (show digest)

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

getAllProcessedFiles :: (MonadUnliftIO m) => Entity User -> m [(Entity ProcessFileJob, Entity UploadedPdf, Maybe (Entity TransactionSource))]
getAllProcessedFiles user = do
  pool <- liftIO getConnectionPool

  -- Fetch all jobs for the given user
  jobs <- runSqlPool (selectList [ProcessFileJobUserId ==. entityKey user, ProcessFileJobArchived ==. False] []) pool

  -- Extract pdfIds from jobs
  let pdfIds = map (processFileJobPdfId . entityVal) jobs

  -- Fetch corresponding PDFs
  pdfs <- runSqlPool (selectList [UploadedPdfId <-. pdfIds, UploadedPdfArchived ==. False] [Asc UploadedPdfFilename]) pool

  -- Extract configIds from jobs (some may be Nothing)
  let configIds = mapMaybe (processFileJobConfigId . entityVal) jobs

  -- Fetch associated UploadConfigurations
  configs <- runSqlPool (selectList [UploadConfigurationId <-. configIds] []) pool

  -- Extract transactionSourceIds from configurations
  let configMap = Data.Map.fromList [(entityKey cfg, cfg) | cfg <- configs]

  -- Fetch associated TransactionSources
  transactionSources <- runSqlPool (selectList [TransactionSourceArchived ==. False, TransactionSourceUserId ==. entityKey user] []) pool

  -- Create lookup maps for fast retrieval
  let pdfMap = Data.Map.fromList [(entityKey pdf, pdf) | pdf <- pdfs]
  let transactionSourceMap = Data.Map.fromList [(entityKey ts, ts) | ts <- transactionSources]

  let results =
        [ (job, pdf, maybeTransactionSource)
          | job <- jobs,
            let pdfId = processFileJobPdfId (entityVal job),
            let maybeConfig =
                  processFileJobConfigId (entityVal job)
                    >>= (`Data.Map.lookup` configMap),
            let maybeTransactionSource =
                  maybeConfig
                    >>= ( flip Data.Map.lookup transactionSourceMap
                            . uploadConfigurationTransactionSourceId
                            . entityVal
                        ),
            Just pdf <- [Data.Map.lookup pdfId pdfMap]
        ]

  return $ Data.List.sortOn sortingKey results
  where
    sortingKey (job, file, maybeSource) =
      (isNothing maybeSource, fmap entityKey maybeSource, uploadedPdfFilename (entityVal file))
