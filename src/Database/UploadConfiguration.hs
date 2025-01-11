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

getAllUploadConfigs :: (MonadUnliftIO m) => m [Entity UploadConfiguration]
getAllUploadConfigs = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUploadConfigs pool
  where
    queryUploadConfigs = selectList [] [Asc UploadConfigurationTransactionSourceId]

getUploadConfiguration :: (MonadUnliftIO m) => Text -> m (Maybe (Entity UploadConfiguration))
getUploadConfiguration filename = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUploadConfiguration pool
  where
    queryUploadConfiguration = do
      results <-
        rawSql
          "SELECT ?? FROM upload_configuration WHERE ? ~ filename_regex"
          [toPersistValue filename]
      return $ listToMaybe results

addUploadConfiguration :: (MonadUnliftIO m) => Text -> Text -> Key TransactionSource -> Text -> m ()
addUploadConfiguration startKeyword endKeyword txnSourceId filenameRegex = do
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
              uploadConfigurationFilenameRegex = Just filenameRegex
            }
      case result of
        Just _ -> return ()
        Nothing -> liftIO $ putStrLn "UploadConfiguration already exists"