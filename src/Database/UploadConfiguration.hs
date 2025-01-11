{-# LANGUAGE OverloadedStrings #-}

module Database.UploadConfiguration
  ( getAllUploadConfigs,
    getUploadConfiguration,
    addUploadConfiguration,
  )
where

import ConnectionPool (getConnectionPool)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.Persist (Entity (..), PersistEntity (Key), SelectOpt (Asc))
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)
import Models

getAllUploadConfigs :: (MonadUnliftIO m) => Entity User -> m [Entity UploadConfiguration]
getAllUploadConfigs user = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUploadConfigs pool
  where
    queryUploadConfigs = selectList [UploadConfigurationUserId ==. entityKey user] [Asc UploadConfigurationTransactionSourceId]

getUploadConfiguration :: (MonadUnliftIO m) => Entity User -> Text -> m (Maybe (Entity UploadConfiguration))
getUploadConfiguration user filename = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUploadConfiguration pool
  where
    queryUploadConfiguration = do
      results <-
        rawSql
          "SELECT ?? FROM upload_configuration WHERE ? ~ filename_regex AND user_id = ?"
          [toPersistValue filename, toPersistValue (entityKey user)]
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
