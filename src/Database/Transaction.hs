{-# LANGUAGE OverloadedStrings #-}

module Database.Transaction
  ( updateTransactionCategory,
    getTransactionsByFileId,
    parseTransactionKind,
    parseTransactionDate,
    groupTransactionsBySource,
    getAllTransactions,
    updateTransaction,
    addTransaction,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.List (nub)
import Data.Map (Map, findWithDefault, fromList, fromListWith, lookup, mapKeys)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Time (Day, UTCTime, defaultTimeLocale, parseTimeM)
import Database.Category
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist (Entity (..), PersistEntity (Key), SelectOpt (Asc))
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)
import Types

updateTransaction :: (MonadUnliftIO m) => Entity User -> Key Transaction -> Maybe Text -> Maybe UTCTime -> Maybe Double -> Maybe (Key Category) -> m ()
updateTransaction user transactionId mDescription mDate mAmount mCategoryId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUpdateTransaction pool
  where
    queryUpdateTransaction = do
      -- Ensure the transaction belongs to the user
      transaction <- get transactionId
      case transaction of
        Just txn
          | transactionUserId txn == entityKey user ->
              let updates =
                    catMaybes
                      [ (TransactionDescription =.) <$> mDescription,
                        (TransactionDateOfTransaction =.) <$> mDate,
                        (TransactionAmount =.) <$> mAmount,
                        (TransactionCategoryId =.) <$> mCategoryId
                      ]
               in update transactionId updates
        _ -> return () -- Skip update if unauthorized or not found

updateTransactionCategory :: (MonadUnliftIO m) => Entity User -> Key Transaction -> Key Category -> m ()
updateTransactionCategory user transactionId newCategoryId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUpdateTransactionCategory pool
  where
    queryUpdateTransactionCategory =
      update transactionId [TransactionCategoryId =. newCategoryId]

getTransactionsByFileId :: (MonadUnliftIO m) => Entity User -> Key UploadedPdf -> m [CategorizedTransaction]
getTransactionsByFileId user fileId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryTransactions pool
  where
    queryTransactions = do
      -- Fetch transactions for the user and uploaded PDF ID
      transactions <-
        selectList
          [ TransactionUploadedPdfId ==. Just fileId,
            TransactionUserId ==. entityKey user
          ]
          [Asc TransactionId]

      -- Fetch transaction sources for the user
      sources <- selectList [TransactionSourceUserId ==. entityKey user] []

      -- Fetch categories for the user
      categories <- selectList [CategoryUserId ==. entityKey user] []

      -- Build a lookup map for sources and categories
      let sourceMap = Data.Map.fromList [(entityKey source, source) | source <- sources]
      let categoryMap = Data.Map.fromList [(entityKey cat, entityVal cat) | cat <- categories]

      -- Construct CategorizedTransaction for each transaction
      catTransactions <- forM transactions $ \txn -> do
        let txnVal = entityVal txn
            txnSource = Data.Map.lookup (transactionTransactionSourceId txnVal) sourceMap
            txnCategory = Data.Map.lookup (transactionCategoryId txnVal) categoryMap

        -- Gracefully handle missing sources or categories
        return $ case (txnSource, txnCategory) of
          (Just source, Just category) ->
            Just
              CategorizedTransaction
                { transaction =
                    txnVal
                      { transactionUserId = entityKey user,
                        transactionUploadedPdfId = Just fileId
                      },
                  category =
                    category
                      { categorySourceId = entityKey source,
                        categoryUserId = entityKey user
                      },
                  transactionId = Just (entityKey txn)
                }
          _ -> Nothing

      return (catMaybes catTransactions)

parseTransactionKind :: Text -> TransactionKind
parseTransactionKind "Withdrawal" = Withdrawal
parseTransactionKind "Deposit" = Deposit
parseTransactionKind _ = error "Invalid transaction kind"

parseTransactionDate :: Text -> Day
parseTransactionDate dateText =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (unpack dateText) of
    Just parsedDate -> parsedDate
    Nothing -> error $ "Error parsing date: " <> unpack dateText

addTransaction :: (MonadUnliftIO m) => Entity User -> CategorizedTransaction -> Key UploadedPdf -> m (Key Transaction)
addTransaction user categorizedTransaction uploadedPdfKey = do
  pool <- liftIO getConnectionPool
  runSqlPool insertTransactionQuery pool
  where
    insertTransactionQuery = do
      let tx = transaction categorizedTransaction
          cat = category categorizedTransaction

      -- Ensure the category exists and get its key
      categoryKey <- ensureCategoryExists user (categoryName cat) (categorySourceId cat)

      -- Insert the transaction
      let newTransaction =
            Transaction
              { transactionDescription = transactionDescription tx,
                transactionCategoryId = categoryKey,
                transactionDateOfTransaction = transactionDateOfTransaction tx,
                transactionAmount = transactionAmount tx,
                transactionTransactionSourceId = categorySourceId cat,
                transactionUploadedPdfId = Just uploadedPdfKey,
                transactionKind = transactionKind tx,
                transactionUserId = entityKey user
              }
      insert newTransaction

getAllTransactions :: (MonadUnliftIO m) => Entity User -> m [CategorizedTransaction]
getAllTransactions user = do
  pool <- liftIO getConnectionPool
  runSqlPool queryAllTransactions pool
  where
    queryAllTransactions = do
      transactions <- selectList [TransactionUserId ==. entityKey user] []

      forM transactions $ \(Entity txnId txn) -> do
        maybeSource <- get (transactionTransactionSourceId txn)
        source <- case maybeSource of
          Just s -> return s
          Nothing -> error $ "TransactionSource not found for ID: " ++ show (transactionTransactionSourceId txn)

        -- Get the associated Category
        maybeCategory <- get (transactionCategoryId txn)
        category <- case maybeCategory of
          Just c -> return c
          Nothing -> error $ "Category not found for ID: " ++ show (transactionCategoryId txn)

        return
          CategorizedTransaction
            { transaction =
                Transaction
                  { transactionDescription = transactionDescription txn,
                    transactionAmount = transactionAmount txn,
                    transactionDateOfTransaction = transactionDateOfTransaction txn,
                    transactionKind = transactionKind txn,
                    transactionUploadedPdfId = transactionUploadedPdfId txn,
                    transactionCategoryId = transactionCategoryId txn,
                    transactionTransactionSourceId = transactionTransactionSourceId txn,
                    transactionUserId = entityKey user
                  },
              category =
                Category
                  { categoryName = categoryName category,
                    categorySourceId = transactionTransactionSourceId txn,
                    categoryUserId = entityKey user
                  },
              transactionId = Just txnId
            }

groupTransactionsBySource ::
  (MonadUnliftIO m) =>
  Entity User ->
  [CategorizedTransaction] ->
  m (Map (Entity TransactionSource) [CategorizedTransaction])
groupTransactionsBySource user categorizedTransactions = do
  pool <- liftIO getConnectionPool
  runSqlPool fetchTransactionSources pool
  where
    fetchTransactionSources = do
      let sourceKeys = nub $ Prelude.map (transactionTransactionSourceId . transaction) categorizedTransactions

      sources <- selectList [TransactionSourceId <-. sourceKeys, TransactionSourceUserId ==. entityKey user] []

      let sourceMap = fromList [(entityKey source, source) | source <- sources]

      let groupedBySourceId =
            fromListWith
              (++)
              [(transactionTransactionSourceId (transaction txn), [txn]) | txn <- categorizedTransactions]
      return $
        mapKeys (\key -> findWithDefault (error "Source not found") key sourceMap) groupedBySourceId
