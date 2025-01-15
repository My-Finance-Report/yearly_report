module Database.TransactionSource
  ( addTransactionSource,
    getTransactionSource,
    updateTransactionSource,
    ensureTransactionSourceExists,
    getAllTransactionSources,
    fetchSourceMap,
    removeTransactionSource,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map (Map, fromList)
import Data.Text (Text, unpack)
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist (Entity (..), PersistEntity (Key), SelectOpt (Asc))
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)

ensureTransactionSourceExists :: Entity User -> Text -> SqlPersistT IO (Key TransactionSource)
ensureTransactionSourceExists user name = do
  maybeSource <- selectFirst [TransactionSourceName ==. name, TransactionSourceUserId ==. entityKey user] []
  case maybeSource of
    Just (Entity sourceId _) -> return sourceId
    Nothing -> insert $ TransactionSource name (entityKey user) False

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
  runSqlPool queryAddTransactionSource pool
  where
    queryAddTransactionSource = do
      maybeArchivedSource <- getBy (UniqueTransactionSource (entityKey user) sourceName)
      case maybeArchivedSource of
        Just (Entity sourceId archivedSource)
          | transactionSourceArchived archivedSource -> do
              update sourceId [TransactionSourceArchived =. False]
              return sourceId
        _ ->
          insert $
            TransactionSource
              { transactionSourceUserId = entityKey user,
                transactionSourceName = sourceName,
                transactionSourceArchived = False
              }

removeTransactionSource :: Entity User -> Text -> IO ()
removeTransactionSource user sourceName = do
  pool <- getConnectionPool
  runSqlPool queryArchiveTransactionSource pool
  where
    queryArchiveTransactionSource = do
      maybeSource <- getBy (UniqueTransactionSource (entityKey user) sourceName)
      case maybeSource of
        Nothing -> liftIO $ putStrLn $ "TransactionSource not found for user: " ++ show (fromSqlKey $ entityKey user)
        Just (Entity sourceId source) -> do
          txCount <- count [TransactionTransactionSourceId ==. sourceId]
          if txCount > 0
            then liftIO $ putStrLn $ "Cannot archive TransactionSource. It is referenced by " ++ show txCount ++ " transactions."
            else do
              update sourceId [TransactionSourceArchived =. True]
              liftIO $ putStrLn $ "TransactionSource '" ++ unpack sourceName ++ "' archived successfully."

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
    queryTransactionSources = selectList [TransactionSourceUserId ==. entityKey user, TransactionSourceArchived ==. False] [Asc TransactionSourceName]

fetchSourceMap :: (MonadUnliftIO m) => Entity User -> m (Map (Key TransactionSource) TransactionSource)
fetchSourceMap user = do
  pool <- liftIO getConnectionPool
  runSqlPool querySourceMap pool
  where
    querySourceMap = do
      sources <- getAllTransactionSources user
      return $ fromList [(entityKey source, entityVal source) | source <- sources]
