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

      -- Fetch all transaction sources and categories for the user
      sources <- selectList [TransactionSourceUserId ==. entityKey user] []
      categories <- selectList [CategoryUserId ==. entityKey user] []

      -- Build lookup maps for sources and categories
      let sourceMap = Data.Map.fromList [(entityKey source, source) | source <- sources]
      let categoryMap = Data.Map.fromList [(entityKey cat, cat) | cat <- categories]
      catTransactions <-
        mapM
          ( \txn -> do
              let txnVal = entityVal txn
                  maybeSource = Data.Map.lookup (transactionTransactionSourceId txnVal) sourceMap
                  maybeCategory = Data.Map.lookup (transactionCategoryId txnVal) categoryMap

              case (maybeSource, maybeCategory) of
                (Just _, Just category) ->
                  return $
                    Just
                      CategorizedTransaction
                        { transaction = txn,
                          category = category,
                          transactionId = Just (entityKey txn)
                        }
                _ -> return Nothing -- Skip incomplete transactions
          )
          transactions

      return $ catMaybes catTransactions

parseTransactionKind :: Text -> TransactionKind
parseTransactionKind "Withdrawal" = Withdrawal
parseTransactionKind "Deposit" = Deposit
parseTransactionKind _ = error "Invalid transaction kind"

parseTransactionDate :: Text -> Day
parseTransactionDate dateText =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (unpack dateText) of
    Just parsedDate -> parsedDate
    Nothing -> error $ "Error parsing date: " <> unpack dateText

addTransaction ::
  (MonadUnliftIO m) =>
  Entity User ->
  Text -> -- Transaction Description
  Double -> -- Transaction Amount
  TransactionKind -> -- Transaction Kind (Deposit/Withdrawal)
  UTCTime -> -- Date of Transaction
  Key UploadedPdf -> -- Uploaded PDF Key
  Key TransactionSource -> -- Transaction Source Key
  Text -> -- Category Name
  Key TransactionSource -> -- Category Source Key
  m (Entity Transaction)
addTransaction user txnDescription txnAmount txnKind txnDate uploadedPdfKey txnSourceKey categoryName categorySourceKey = do
  pool <- liftIO getConnectionPool
  runSqlPool insertTransactionQuery pool
  where
    insertTransactionQuery = do
      -- Ensure the category exists and get its key
      categoryKey <- ensureCategoryExists user categoryName categorySourceKey

      -- Construct the new transaction
      let newTransaction =
            Transaction
              { transactionDescription = txnDescription,
                transactionCategoryId = categoryKey,
                transactionDateOfTransaction = txnDate,
                transactionAmount = txnAmount,
                transactionTransactionSourceId = txnSourceKey,
                transactionUploadedPdfId = Just uploadedPdfKey,
                transactionKind = txnKind,
                transactionUserId = entityKey user
              }

      -- Insert the transaction and return as Entity
      transactionKey <- insert newTransaction
      return $ Entity transactionKey newTransaction

getAllTransactions :: (MonadUnliftIO m) => Entity User -> m [CategorizedTransaction]
getAllTransactions user = do
  pool <- liftIO getConnectionPool
  runSqlPool queryAllTransactions pool
  where
    queryAllTransactions = do
      transactions <- selectList [TransactionUserId ==. entityKey user] []

      forM transactions $ \txn -> do
        maybeEntityCategory <- getEntity (transactionCategoryId (entityVal txn))
        category <- case maybeEntityCategory of
          Just c -> return c
          Nothing -> error $ "Category not found for ID: " ++ show (transactionCategoryId (entityVal txn))

        return
          CategorizedTransaction
            { transaction = txn,
              category = category,
              transactionId = Just (entityKey txn)
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
      let sourceKeys = nub $ Prelude.map (transactionTransactionSourceId . entityVal . transaction) categorizedTransactions

      sources <- selectList [TransactionSourceId <-. sourceKeys, TransactionSourceUserId ==. entityKey user] []

      let sourceMap = fromList [(entityKey source, source) | source <- sources]

      let groupedBySourceId =
            fromListWith
              (++)
              [(transactionTransactionSourceId (entityVal (transaction txn)), [txn]) | txn <- categorizedTransactions]
      return $
        mapKeys (\key -> findWithDefault (error "Source not found") key sourceMap) groupedBySourceId
