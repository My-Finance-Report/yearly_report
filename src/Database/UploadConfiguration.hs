{-# LANGUAGE OverloadedStrings #-}

module Database.UploadConfiguration
  ( getAllUploadConfigs,
    getUploadConfiguration,
    getUploadConfigurationFromPdf,
    addUploadConfiguration,
    addUploadConfigurationObject,
    getUploadConfigById,
  )
where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist (Entity (..), PersistEntity (Key), SelectOpt (Asc))
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)

getAllUploadConfigs :: (MonadUnliftIO m) => Entity User -> m [Entity UploadConfiguration]
getAllUploadConfigs user = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUploadConfigs pool
  where
    queryUploadConfigs = selectList [UploadConfigurationUserId ==. entityKey user] [Asc UploadConfigurationTransactionSourceId]

getUploadConfigById :: (MonadUnliftIO m) => Entity User -> Key UploadConfiguration -> m (Entity UploadConfiguration)
getUploadConfigById user configId = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool queryUploadConfig pool
  liftIO $ maybe (throwIO $ userError "Upload config not found") pure result
  where
    queryUploadConfig = selectFirst [UploadConfigurationUserId ==. entityKey user, UploadConfigurationId ==. configId] []

getUploadConfigurationFromPdf :: (MonadUnliftIO m) => Entity User -> Key UploadedPdf -> m (Maybe (Entity UploadConfiguration))
getUploadConfigurationFromPdf user pdfId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUploadConfiguration pool
  where
    queryUploadConfiguration = do
      maybePdf <- get pdfId
      case maybePdf of
        Nothing -> return Nothing
        Just pdf -> do
          let filename = uploadedPdfFilename pdf
          let rawText = uploadedPdfRawContent pdf

          results <-
            rawSql
              "SELECT ?? FROM upload_configuration WHERE ? ~* filename_regex AND user_id = ?"
              [toPersistValue (filename <> rawText), toPersistValue (entityKey user)]

          return $ listToMaybe results

getUploadConfiguration :: (MonadUnliftIO m) => Entity User -> Text -> Text -> m (Maybe (Entity UploadConfiguration))
getUploadConfiguration user filename rawText = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUploadConfiguration pool
  where
    queryUploadConfiguration = do
      results <-
        rawSql
          "SELECT ?? FROM upload_configuration WHERE ? ~* filename_regex AND user_id = ?"
          [toPersistValue (filename <> rawText), toPersistValue (entityKey user)]
      return $ listToMaybe results

addUploadConfiguration :: (MonadUnliftIO m) => Entity User -> Text -> Text -> Key TransactionSource -> Text -> m ()
addUploadConfiguration user startKeyword endKeyword txnSourceId filenameRegex = do
  pool <- liftIO getConnectionPool
  runSqlPool queryPersistUploadConfiguration pool
  where
    queryPersistUploadConfiguration = do
      result <-
        insertUnique $
          UploadConfiguration
            { uploadConfigurationStartKeyword = Just startKeyword,
              uploadConfigurationEndKeyword = Just endKeyword,
              uploadConfigurationTransactionSourceId = txnSourceId,
              uploadConfigurationFilenameRegex = Just filenameRegex,
              uploadConfigurationUserId = entityKey user
            }
      case result of
        Just _ -> liftIO $ putStrLn "UploadConfiguration added successfully"
        Nothing -> liftIO $ putStrLn $ "UploadConfiguration already exists for user " ++ show (fromSqlKey $ entityKey user)

addUploadConfigurationObject :: (MonadUnliftIO m) => Entity User -> UploadConfiguration -> m ()
addUploadConfigurationObject user config = do
  let userIdInConfig = uploadConfigurationUserId config
  let passedUserId = entityKey user

  when (userIdInConfig /= passedUserId) $
    error $
      "User ID mismatch: config userId "
        ++ show (fromSqlKey userIdInConfig)
        ++ " does not match passed userId "
        ++ show (fromSqlKey passedUserId)

  pool <- liftIO getConnectionPool
  runSqlPool queryPersistUploadConfiguration pool
  where
    queryPersistUploadConfiguration = do
      result <-
        insertUnique config

      case result of
        Just _ -> liftIO $ putStrLn "UploadConfiguration added successfully"
        Nothing -> liftIO $ putStrLn $ "UploadConfiguration already exists for user " ++ show (fromSqlKey $ entityKey user)
