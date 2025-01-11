{-# LANGUAGE OverloadedStrings #-}

module Database.Transaction(
    updateTransactionCategory,
    getTransactionsByFileId,
    parseTransactionKind,
    parseTransactionDate,
    groupTransactionsBySource, 
    getAllTransactions,
    addTransaction,

) where

import Control.Monad (forM, forM_)
import ConnectionPool (getConnectionPool)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text, unpack)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.List (nub)
import Data.Map (Map, findWithDefault, fromListWith, mapKeys, fromList)
import Database.Category
import Database.Persist (Entity (..), PersistEntity (Key), SelectOpt (Asc))
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)
import Models
import Types

updateTransactionCategory :: (MonadUnliftIO m) => Entity User ->Key Transaction -> Key Category -> m ()
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
      results <-
        rawSql
          "SELECT ??, ??, ?? \
          \FROM \"transaction\" t \
          \INNER JOIN \"transaction_source\" ts ON t.transaction_source_id = ts.id \
          \INNER JOIN \"category\" c ON t.category_id = c.id \
          \WHERE t.uploaded_pdf_id = ?"
          [toPersistValue fileId]

      forM results $ \(Entity txnId txn, Entity sourceId source, Entity catId cat) -> do
        return
          CategorizedTransaction
            { transaction =
                Transaction
                  { transactionDescription = transactionDescription txn,
                    transactionAmount = transactionAmount txn,
                    transactionDateOfTransaction = transactionDateOfTransaction txn,
                    transactionKind = transactionKind txn,
                    transactionUploadedPdfId = Just fileId,
                    transactionCategoryId = catId,
                    transactionTransactionSourceId = sourceId,
                    transactionUserId = entityKey user
                  },
              category =
                Category
                  { categoryName = categoryName cat,
                    categorySourceId = sourceId,
                    categoryUserId = entityKey user
                  },
              transactionId = Just txnId
            }

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
      transactions <- selectList  [TransactionUserId ==. entityKey user] []

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