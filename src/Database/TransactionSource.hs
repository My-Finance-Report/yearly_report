module Database.TransactionSource
  ( addTransactionSource,
    getTransactionSource,
    updateTransactionSource,
    ensureTransactionSourceExists,
    getAllTransactionSources,
  )
where

import ConnectionPool (getConnectionPool)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import Database.Persist (Entity (..), PersistEntity (Key), SelectOpt (Asc))
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)
import Models

ensureTransactionSourceExists :: Entity User -> Text -> SqlPersistT IO (Key TransactionSource)
ensureTransactionSourceExists user name = do
  maybeSource <- selectFirst [TransactionSourceName ==. name, TransactionSourceUserId ==. entityKey user] []
  case maybeSource of
    Just (Entity sourceId _) -> return sourceId
    Nothing -> insert $ TransactionSource name (entityKey user)

getTransactionSource :: (MonadUnliftIO m) => Entity User -> Key TransactionSource -> m (Entity TransactionSource)
getTransactionSource user sourceId = do
  pool <- liftIO getConnectionPool
  result <-
    runSqlPool
      (selectFirst [TransactionSourceId ==. sourceId, TransactionSourceUserId ==. entityKey user] [])
      pool
  case result of
    Just (Entity _ source) -> return $ Entity sourceId source
    Nothing -> liftIO $ error $ "No transaction source found with ID: " ++ show (fromSqlKey sourceId) ++ " for user ID: " ++ show (fromSqlKey $ entityKey user)

addTransactionSource :: Entity User -> Text -> IO (Key TransactionSource)
addTransactionSource user sourceName = do
  pool <- getConnectionPool
  runSqlPool (insert $ TransactionSource {transactionSourceUserId = entityKey user, transactionSourceName = sourceName}) pool

updateTransactionSource :: (MonadUnliftIO m) => Entity User -> Key TransactionSource -> Text -> m ()
updateTransactionSource user sourceId newName = do
  pool <- liftIO getConnectionPool
  runSqlPool
    ( do
        maybeSource <-
          selectFirst
            [TransactionSourceId ==. sourceId, TransactionSourceUserId ==. entityKey user]
            []
        case maybeSource of
          Just _ -> update sourceId [TransactionSourceName =. newName]
          Nothing -> liftIO $ error $ "No transaction source found with ID: " ++ show (fromSqlKey sourceId) ++ " for user ID: " ++ show (fromSqlKey $ entityKey user)
    )
    pool

getAllTransactionSources :: (MonadUnliftIO m) => Entity User -> m [Entity TransactionSource]
getAllTransactionSources user = do
  pool <- liftIO getConnectionPool
  runSqlPool queryTransactionSources pool
  where
    queryTransactionSources = selectList [TransactionSourceUserId ==. entityKey user] [Asc TransactionSourceName]