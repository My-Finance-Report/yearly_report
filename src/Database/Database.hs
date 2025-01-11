{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Database
  ( seedDatabase,
    getAllFilenames,
    getTransactionsByFileId,
    getAllUploadConfigs,
    getUploadConfiguration,
    insertTransaction,
    getFirstSankeyConfig,
    fetchPdfRecord,
    isFileProcessed,
    markFileAsProcessed,
    getAllTransactions,
    groupTransactionsBySource,
    saveSankeyConfig,
    persistUploadConfiguration,
    insertPdfRecord,
    updateTransactionCategory,
    parseTransactionKind,
    getSourceFileMappings,
    fetchSourceMap,
  )
where

import ConnectionPool
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (unliftIO))
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.Map hiding (insert, update)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Category (ensureCategoriesExist, ensureCategoryExists)
import Database.Persist (Entity (..), Filter)
import Database.Persist.Postgresql (insert, rawSql, runSqlPool, selectFirst, (==.))
import Database.Persist.Sql
import Database.TransactionSource (ensureTransactionSourceExists)
import Models
import Types

seedDatabase :: Entity User -> IO ()
seedDatabase user = do
  pool <- getConnectionPool
  runSqlPool
    ( do
        -- Create or get transaction sources
        bankSourceId <- ensureTransactionSourceExists user "Bank"
        ccSourceId <- ensureTransactionSourceExists user "CreditCard"

        -- Create categories for the bank source
        ensureCategoriesExist
          user
          bankSourceId
          ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]

        -- Create categories for the credit card source
        ensureCategoriesExist
          user
          ccSourceId
          ["Groceries", "Travel", "Gas", "Misc", "Subscriptions", "Food"]

        liftIO $ putStrLn "Database seeded successfully!"
    )
    pool

-- Ensure a transaction source exists, returning its ID

updateTransactionCategory :: (MonadUnliftIO m) => Key Transaction -> Key Category -> m ()
updateTransactionCategory transactionId newCategoryId = do
  pool <- liftIO getConnectionPool
  runSqlPool queryUpdateTransactionCategory pool
  where
    queryUpdateTransactionCategory =
      update transactionId [TransactionCategoryId =. newCategoryId]

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

getAllFilenames :: (MonadUnliftIO m) => m [Text]
getAllFilenames = do
  pool <- liftIO getConnectionPool
  runSqlPool queryFilenames pool
  where
    queryFilenames = do
      results <- selectList [] [Asc UploadedPdfId]
      return $ Prelude.map (uploadedPdfFilename . entityVal) results

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
                    transactionTransactionSourceId = sourceId
                  },
              category =
                Category
                  { categoryName = categoryName cat,
                    categorySourceId = sourceId,
                    categoryUserId = entityKey user
                  },
              transactionId = Just txnId
            }

    parseTransactionDate :: Text -> Day
    parseTransactionDate dateText =
      case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateText) of
        Just parsedDate -> parsedDate
        Nothing -> error $ "Error parsing date: " <> T.unpack dateText

    parseTransactionKind :: Text -> TransactionKind
    parseTransactionKind "Withdrawal" = Withdrawal
    parseTransactionKind "Deposit" = Deposit
    parseTransactionKind _ = error "Invalid transaction kind"

insertTransaction :: (MonadUnliftIO m) => Entity User -> CategorizedTransaction -> Key UploadedPdf -> m (Key Transaction)
insertTransaction user categorizedTransaction uploadedPdfKey = do
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
                transactionKind = transactionKind tx
              }
      insert newTransaction

getFirstSankeyConfig :: (MonadUnliftIO m) => m (Maybe FullSankeyConfig)
getFirstSankeyConfig = do
  pool <- liftIO getConnectionPool
  liftIO $ runSqlPool queryFirstSankeyConfig pool
  where
    queryFirstSankeyConfig :: SqlPersistT IO (Maybe FullSankeyConfig)
    queryFirstSankeyConfig = do
      -- Fetch the first SankeyConfig
      maybeConfig <- selectFirst [] []
      case maybeConfig of
        Nothing -> return Nothing
        Just (Entity configId SankeyConfig {sankeyConfigName = configName}) -> do
          -- Fetch the inputs for the config
          inputEntities <- selectList [SankeyInputConfigId ==. configId] []

          -- Fetch the linkages for the config
          linkageEntities <- selectList [SankeyLinkageConfigId ==. configId] []

          -- Resolve inputs into entities for TransactionSource and Category
          inputs <- fmap catMaybes $ forM inputEntities $ \(Entity _ (SankeyInput _ sourceId categoryId)) -> do
            maybeSource <- getEntity sourceId
            maybeCategory <- getEntity categoryId
            return $ (,) <$> maybeSource <*> maybeCategory

          -- Resolve the linkage into entities
          linkage <- case linkageEntities of
            [Entity _ (SankeyLinkage _ sourceId categoryId targetId)] -> do
              maybeSource <- getEntity sourceId
              maybeCategory <- getEntity categoryId
              maybeTarget <- getEntity targetId
              return $ (,,) <$> maybeSource <*> maybeCategory <*> maybeTarget
            _ -> return Nothing

          -- Construct and return the FullSankeyConfig if all components are valid
          case linkage of
            Just linkageTriple ->
              return $
                Just
                  FullSankeyConfig
                    { inputs = inputs,
                      linkages = linkageTriple,
                      mapKeyFunction = transactionSourceName . entityVal,
                      configName = configName
                    }
            Nothing -> return Nothing

fetchPdfRecord :: (MonadUnliftIO m) => Key UploadedPdf -> m UploadedPdf
fetchPdfRecord pdfId = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool (get pdfId) pool
  case result of
    Just pdf -> return pdf
    Nothing -> liftIO $ error $ "No PDF found with id=" ++ show (fromSqlKey pdfId)

insertPdfRecord :: (MonadUnliftIO m) => T.Text -> T.Text -> T.Text -> m (Key UploadedPdf)
insertPdfRecord filename rawContent uploadTime = do
  pool <- liftIO getConnectionPool
  runSqlPool queryInsertPdfRecord pool
  where
    queryInsertPdfRecord = do
      insert $
        UploadedPdf
          { uploadedPdfFilename = filename,
            uploadedPdfRawContent = rawContent,
            uploadedPdfUploadTime = uploadTime
          }

isFileProcessed :: (MonadUnliftIO m) => Text -> m Bool
isFileProcessed filename = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool (selectFirst [ProcessedFileFilename ==. filename] []) pool
  return $ isJust result

markFileAsProcessed :: (MonadUnliftIO m) => Text -> m ()
markFileAsProcessed filename = do
  pool <- liftIO getConnectionPool
  result <- runSqlPool (insertUnique $ ProcessedFile filename) pool
  case result of
    Just _ -> liftIO $ putStrLn $ "File processed: " <> T.unpack filename
    Nothing -> liftIO $ putStrLn $ "File already marked as processed: " <> T.unpack filename

getAllTransactions :: (MonadUnliftIO m) => Entity User -> m [CategorizedTransaction]
getAllTransactions user = do
  pool <- liftIO getConnectionPool
  runSqlPool queryAllTransactions pool
  where
    queryAllTransactions = do
      -- Fetch all transactions
      transactions <- selectList [] []

      -- For each transaction, fetch its associated TransactionSource and Category
      forM transactions $ \(Entity txnId txn) -> do
        -- Get the associated TransactionSource
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
                    transactionTransactionSourceId = transactionTransactionSourceId txn
                  },
              category =
                Category
                  { categoryName = categoryName category,
                    categorySourceId = transactionTransactionSourceId txn,
                    categoryUserId = entityKey user
                  },
              transactionId = Just txnId
            }

parseTransactionDate :: Text -> Day
parseTransactionDate dateText =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateText) of
    Just parsedDate -> parsedDate
    Nothing -> error $ "Error parsing date: " <> T.unpack dateText

parseTransactionKind :: Text -> TransactionKind
parseTransactionKind "Withdrawal" = Withdrawal
parseTransactionKind "Deposit" = Deposit
parseTransactionKind _ = error "Invalid transaction kind"

groupTransactionsBySource ::
  (MonadUnliftIO m) =>
  [CategorizedTransaction] ->
  m (Map (Entity TransactionSource) [CategorizedTransaction])
groupTransactionsBySource categorizedTransactions = do
  pool <- liftIO getConnectionPool
  runSqlPool fetchTransactionSources pool
  where
    fetchTransactionSources = do
      -- Get unique TransactionSource keys from transactions
      let sourceKeys = nub $ Prelude.map (transactionTransactionSourceId . transaction) categorizedTransactions
      -- Fetch all corresponding TransactionSource entities
      sources <- selectList [TransactionSourceId <-. sourceKeys] []
      -- Build a Map from TransactionSourceId to Entity TransactionSource
      let sourceMap = fromList [(entityKey source, source) | source <- sources]
      -- Group transactions by TransactionSourceId and map to TransactionSource entities
      let groupedBySourceId =
            fromListWith
              (++)
              [(transactionTransactionSourceId (transaction txn), [txn]) | txn <- categorizedTransactions]
      -- Map TransactionSourceId keys to Entity TransactionSource keys
      return $
        mapKeys (\key -> findWithDefault (error "Source not found") key sourceMap) groupedBySourceId

saveSankeyConfig :: (MonadUnliftIO m) => FullSankeyConfig -> m (Key SankeyConfig)
saveSankeyConfig config = do
  pool <- liftIO getConnectionPool
  liftIO $ runSqlPool saveConfigQuery pool
  where
    saveConfigQuery :: SqlPersistT IO (Key SankeyConfig)
    saveConfigQuery = do
      -- Upsert into sankey_config
      sankeyConfigId <- upsertSankeyConfig

      -- Clear existing inputs and linkages
      deleteWhere [SankeyInputConfigId ==. sankeyConfigId]
      deleteWhere [SankeyLinkageConfigId ==. sankeyConfigId]

      -- Insert new inputs
      forM_ (inputs config) $ \(Entity sourceId _, Entity categoryId _) ->
        insert_ $ SankeyInput sankeyConfigId sourceId categoryId

      -- Insert new linkage
      let (Entity sourceId _, Entity categoryId _, Entity targetSourceId _) = linkages config
      insert_ $ SankeyLinkage sankeyConfigId sourceId categoryId targetSourceId

      return sankeyConfigId

    -- Helper to upsert into `sankey_config`
    upsertSankeyConfig :: SqlPersistT IO (Key SankeyConfig)
    upsertSankeyConfig = do
      existing <- selectFirst [SankeyConfigName ==. configName config] []
      case existing of
        Just (Entity key _) -> do
          update key [SankeyConfigName =. configName config]
          return key
        Nothing -> insert $ SankeyConfig (configName config)

persistUploadConfiguration :: (MonadUnliftIO m) => T.Text -> T.Text -> Key TransactionSource -> T.Text -> m ()
persistUploadConfiguration startKeyword endKeyword txnSourceId filenameRegex = do
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
        Just _ -> return () -- Successfully inserted
        Nothing -> liftIO $ putStrLn "UploadConfiguration already exists" -- Log if already exists

getSourceFileMappings :: (MonadUnliftIO m) => m [SourceFileMapping]
getSourceFileMappings = do
  pool <- liftIO getConnectionPool
  runSqlPool querySourceFileMappings pool
  where
    querySourceFileMappings = do
      -- Fetch all transaction sources, transactions, and uploaded PDFs
      sources <- selectList [] []
      transactions <- selectList [] []
      pdfs <- selectList [] []

      -- Convert PDFs to a Map for efficient lookups
      let pdfMap = Map.fromList [(entityKey pdf, entityVal pdf) | pdf <- pdfs]

      -- Group transactions by source with unique filenames
      let sourceToFiles =
            Map.fromListWith
              Set.union
              [ (transactionTransactionSourceId txn, Set.singleton (uploadedPdfFilename pdf))
                | Entity _ txn <- transactions,
                  Just pdfId <- [transactionUploadedPdfId txn],
                  Just pdf <- [Map.lookup pdfId pdfMap]
              ]

      -- Convert the Set of filenames back to a list for `handledFiles`
      return
        [ SourceFileMapping
            { source = source,
              handledFiles = Set.toList $ Map.findWithDefault Set.empty (entityKey source) sourceToFiles
            }
          | source <- sources
        ]

fetchSourceMap :: (MonadUnliftIO m) => m (Map (Key TransactionSource) TransactionSource)
fetchSourceMap = do
  pool <- liftIO getConnectionPool
  runSqlPool querySourceMap pool
  where
    querySourceMap = do
      sources <- selectList [] []
      return $ Map.fromList [(entityKey source, entityVal source) | source <- sources]
