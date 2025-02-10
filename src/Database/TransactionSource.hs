{-# LANGUAGE OverloadedStrings #-}

module Database.TransactionSource
  ( addTransactionSource,
    addTransactionSourceWithCategories,
    getTransactionSource,
    updateTransactionSource,
    ensureTransactionSourceExists,
    getAllTransactionSources,
    fetchSourceMap,
    removeTransactionSource,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map (Map, fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist (Entity (..), PersistEntity (Key), SelectOpt (Asc))
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)

ensureTransactionSourceExists :: Entity User -> Text -> SourceKind -> SqlPersistT IO (Key TransactionSource)
ensureTransactionSourceExists user name kind = do
  maybeSource <- selectFirst [TransactionSourceName ==. name, TransactionSourceUserId ==. entityKey user] []
  case maybeSource of
    Just (Entity sourceId _) -> return sourceId
    Nothing ->
      insert $
        TransactionSource
          { transactionSourceUserId = entityKey user,
            transactionSourceName = name,
            transactionSourceArchived = False,
            transactionSourceSourceKind = kind
          }

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

defaultCategoriesForSource :: SourceKind -> [Text]
defaultCategoriesForSource Account =
  ["Income", "Investments", "Credit Card Payments", "Transfers", "Housing"]
defaultCategoriesForSource Card =
  ["Groceries", "Travel", "Gas", "Insurance", "Misc", "Subscriptions", "Credit Card Payments", "Entertainment"]
defaultCategoriesForSource Investment =
  ["Stocks", "Bonds", "Real Estate", "Crypto", "Index Funds"]

addTransactionSourceWithCategories ::
  Entity User ->
  Text ->
  SourceKind ->
  [Text] ->
  IO (Maybe (Key TransactionSource))
addTransactionSourceWithCategories user sourceName kind categoriesToAdd = do
  pool <- getConnectionPool
  runSqlPool queryAddTransactionSource pool
  where
    queryAddTransactionSource = do
      maybeSource <- getBy (UniqueTransactionSource (entityKey user) sourceName)
      case maybeSource of
        -- If the source exists but is archived, unarchive it
        Just (Entity sourceId archivedSource)
          | transactionSourceArchived archivedSource -> do
              update sourceId [TransactionSourceArchived =. False]
              addOrUnarchiveCategories categoriesToAdd sourceId
              return (Just sourceId)

        -- If the source exists and is not archived, return an error
        Just _ -> return Nothing
        -- Otherwise, create a new transaction source
        Nothing -> do
          newSourceId <-
            insert $
              TransactionSource
                { transactionSourceUserId = entityKey user,
                  transactionSourceName = sourceName,
                  transactionSourceArchived = False,
                  transactionSourceSourceKind = kind
                }

          -- Insert provided + default categories
          addOrUnarchiveCategories categoriesToAdd newSourceId

          return (Just newSourceId)

    -- \| Inserts or unarchives categories for a source
    addOrUnarchiveCategories :: [Text] -> Key TransactionSource -> SqlPersistT IO ()
    addOrUnarchiveCategories categoryNames sourceId =
      forM_ categoryNames $ \categoryName -> do
        maybeCategory <- getBy $ UniqueCategory categoryName sourceId
        case maybeCategory of
          Just (Entity categoryId category) -> do
            when (categoryArchived category) $
              update categoryId [CategoryArchived =. False]
          Nothing -> do
            insert_ $ Category categoryName sourceId (entityKey user) False

addTransactionSource :: Entity User -> Text -> SourceKind -> IO (Maybe (Key TransactionSource))
addTransactionSource user sourceName kind = do
  pool <- getConnectionPool
  runSqlPool queryAddTransactionSource pool


  where
    queryAddTransactionSource = do
      maybeSource <- getBy (UniqueTransactionSource (entityKey user) sourceName)
      case maybeSource of
        -- If the source exists but is archived, unarchive it
        Just (Entity sourceId archivedSource)
          | transactionSourceArchived archivedSource -> do
              update sourceId [TransactionSourceArchived =. False]

              let defaultCategories = defaultCategoriesForSource kind
              forM_ defaultCategories $ \categoryName -> do
                _ <- queryAddOrUnarchiveCategory categoryName sourceId
                return ()

              return (Just sourceId)

        -- If the source exists and is not archived, return an error
        Just _ -> return (fmap entityKey maybeSource)
        -- Otherwise, create a new transaction source
        Nothing -> do
          newSourceId <-
            insert $
              TransactionSource
                { transactionSourceUserId = entityKey user,
                  transactionSourceName = sourceName,
                  transactionSourceArchived = False,
                  transactionSourceSourceKind = kind
                }

          -- Insert default categories
          let defaultCategories = defaultCategoriesForSource kind
          forM_ defaultCategories $ \categoryName -> do
            _ <- queryAddOrUnarchiveCategory categoryName newSourceId
            return ()

          return (Just newSourceId)

    -- Add category if it does not exist, otherwise unarchive it
    queryAddOrUnarchiveCategory categoryName sourceId = do
      maybeCategory <- getBy $ UniqueCategory categoryName sourceId
      case maybeCategory of
        Just (Entity categoryId category) -> do
          when (categoryArchived category) $
            update categoryId [CategoryArchived =. False]
          return categoryId
        Nothing -> do
          insert $ Category categoryName sourceId (entityKey user) False

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

updateTransactionSource :: (MonadUnliftIO m) => Entity User -> Key TransactionSource -> Maybe SourceKind -> Text -> m ()
updateTransactionSource user sourceId maybeKind newName = do
  pool <- liftIO getConnectionPool
  runSqlPool
    ( do
        maybeSource <-
          selectFirst
            [TransactionSourceId ==. sourceId, TransactionSourceUserId ==. entityKey user]
            []
        case (maybeSource, maybeKind) of
          (Just _, Nothing) -> update sourceId [TransactionSourceName =. newName]
          (Just _, Just kind) -> update sourceId [TransactionSourceName =. newName, TransactionSourceSourceKind =. kind]
          (Nothing, Nothing) -> liftIO $ error $ "No transaction source found with ID: " ++ show (fromSqlKey sourceId) ++ " for user ID: " ++ show (fromSqlKey $ entityKey user)
    )
    pool

getAllTransactionSources :: (MonadUnliftIO m) => Entity User -> m [Entity TransactionSource]
getAllTransactionSources user = do
  pool <- liftIO getConnectionPool
  runSqlPool queryTransactionSources pool
  where
    queryTransactionSources = selectList [TransactionSourceUserId ==. entityKey user, TransactionSourceArchived ==. False] [Asc TransactionSourceId]

fetchSourceMap :: (MonadUnliftIO m) => Entity User -> m (Map (Key TransactionSource) TransactionSource)
fetchSourceMap user = do
  pool <- liftIO getConnectionPool
  runSqlPool querySourceMap pool
  where
    querySourceMap = do
      sources <- getAllTransactionSources user
      return $ fromList [(entityKey source, entityVal source) | source <- sources]
