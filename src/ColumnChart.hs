{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ColumnChart (generateColChartData) where

import Control.Exception (Exception, throw, throwIO)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Data.Aeson hiding (Key)
import Data.List (sortOn)
import Data.Map (Map, elems, findWithDefault, fromListWith, keys, keysSet, singleton, toList, unionWith)
import qualified Data.Map
import Data.Set (Set, unions)
import qualified Data.Set
import Data.Text (Text, pack)
import Data.Time (UTCTime (..), defaultTimeLocale, formatTime, fromGregorian, parseTimeM, toGregorian)
import Database.ConnectionPool (getConnectionPool)
import Database.Models
import Database.Persist
import Database.Persist.Postgresql (SqlPersistT, runSqlPool)
import Database.TransactionSource
import GHC.Generics (Generic)
import Types

data Matrix = Matrix
  { columnHeaders :: [Text],
    rowHeaders :: [Text],
    dataRows :: [[Double]]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Matrix

-- Truncate to the first day of the month
truncateToMonth :: UTCTime -> UTCTime
truncateToMonth utcTime =
  let (year, month, _) = toGregorian (utctDay utcTime)
   in UTCTime (fromGregorian year month 1) 0

-- Format date as "MMM YYYY" (e.g., "Jan 2024")
formatMonthYear :: UTCTime -> Text
formatMonthYear utcTime = pack (formatTime defaultTimeLocale "%b %Y" utcTime)

-- Check if transaction is a withdrawal
isWithdrawal :: CategorizedTransaction -> Bool
isWithdrawal txn =
  case transactionKind $ entityVal (transaction txn) of
    Deposit -> False
    Withdrawal -> True

-- Get withdrawal amount (0 for deposits)
getWithdrawalAmount :: CategorizedTransaction -> Double
getWithdrawalAmount txn =
  case transactionKind $ entityVal (transaction txn) of
    Deposit -> 0
    Withdrawal -> transactionAmount $ entityVal $ transaction txn

-- Generate the group key: (Source Name - Category Name)
resolveSourceCategoryName ::
  Map (Key TransactionSource) TransactionSource ->
  CategorizedTransaction ->
  Text
resolveSourceCategoryName sourceMap txn =
  let sourceId = categorySourceId $ entityVal $ category txn
      catName = categoryName $ entityVal $ category txn
   in catName

-- Group withdrawals by (Month, Source, Category)
groupBySourceCategoryAndMonth ::
  Maybe (Key TransactionSource) -> -- Optional filter for a specific source
  Map (Key TransactionSource) TransactionSource ->
  [CategorizedTransaction] ->
  Map UTCTime (Map Text Double)
groupBySourceCategoryAndMonth selectedSourceId sourceMap txns =
  fromListWith
    (unionWith (+))
    [ ( truncateToMonth $ transactionDateOfTransaction $ entityVal $ transaction txn,
        singleton (resolveSourceCategoryName sourceMap txn) (getWithdrawalAmount txn)
      )
      | txn <- txns,
        isWithdrawal txn,
        case selectedSourceId of
          Just sourceId -> categorySourceId (entityVal (category txn)) == sourceId -- Filter by source
          Nothing -> True -- No filtering, include all transactions
    ]

-- Extract all unique (Source, Category) pairs from grouped data
extractAllSourceCategoryNames :: Map UTCTime (Map Text Double) -> [Text]
extractAllSourceCategoryNames groupedData =
  Data.Set.toList $ Data.Set.unions (map Data.Map.keysSet $ Data.Map.elems groupedData)

-- Build matrix for stacked column chart
buildMatrix ::
  [Text] -> -- All (Source, Category) pairs
  Map UTCTime (Map Text Double) -> -- Grouped data by (Month, Source, Category)
  Matrix
buildMatrix allSourceCategoryNames groupedData =
  let columnHeaders = "Month" : allSourceCategoryNames
      rowHeaders = map (formatMonthYear . fst) (sortOn fst (Data.Map.toList groupedData))
      dataRows = map (buildRow allSourceCategoryNames) (sortOn fst (Data.Map.toList groupedData))
   in Matrix columnHeaders rowHeaders dataRows

buildRow ::
  [Text] -> -- All unique (Source, Category) pairs
  (UTCTime, Map Text Double) -> -- Data for one month
  [Double]
buildRow allSourceCategoryNames (_, sources) =
  map (\sourceCategory -> findWithDefault 0 sourceCategory sources) allSourceCategoryNames

filterByAllowedSources :: [TransactionSource] -> Map (Key TransactionSource) TransactionSource -> Map (Key TransactionSource) TransactionSource
filterByAllowedSources allowedSources = Data.Map.filter (`elem` allowedSources)

setActiveConfigInner :: (MonadUnliftIO m) => Entity User -> Key ColChartConfig -> SqlPersistT m ()
setActiveConfigInner user newActiveId = do
  updateWhere
    [ColChartConfigUserId ==. entityKey user, ColChartConfigActive ==. True]
    [ColChartConfigActive =. False]

  update newActiveId [ColChartConfigActive =. True]

data ConfigNotFoundException = ConfigNotFoundException deriving (Show)

instance Exception ConfigNotFoundException

selectDefaultConfig :: (MonadUnliftIO m) => Entity User -> m (Entity ColChartConfig)
selectDefaultConfig user = do
  pool <- liftIO getConnectionPool
  maybeConfig <- runSqlPool querySources pool
  case maybeConfig of
    Just config -> return config
    Nothing -> throw ConfigNotFoundException -- Use a proper exception instead of a raw string
  where
    querySources = selectFirst [ColChartConfigUserId ==. entityKey user, ColChartConfigActive ==. True] []

sourcesFromConfig :: (MonadUnliftIO m) => Entity User -> Entity ColChartConfig -> m [TransactionSource]
sourcesFromConfig user config = do
  pool <- liftIO getConnectionPool
  runSqlPool querySources pool
  where
    querySources = do
      -- Get all ColChartInput records for this config
      colChartInputs <- selectList [ColchartInputConfigId ==. entityKey config] []

      -- Extract TransactionSourceIds
      let sourceIds = map (colchartInputSourceId . entityVal) colChartInputs

      -- Fetch TransactionSources corresponding to those IDs
      sources <- selectList [TransactionSourceId <-. sourceIds, TransactionSourceUserId ==. entityKey user] []
      return $ map entityVal sources

generateColChartData :: (MonadUnliftIO m) => Entity User -> [CategorizedTransaction] -> Maybe (Key TransactionSource) -> m Matrix
generateColChartData user transactions source = do
  sourceMap <- fetchSourceMap user
  -- TODO
  -- config <- selectDefaultConfig user
  -- allowedSources <- sourcesFromConfig user config
  -- let filteredSourceMap = filterByAllowedSources allowedSources sourceMap
  let grouped = groupBySourceCategoryAndMonth source sourceMap transactions
  let allowedSourceNames = extractAllSourceCategoryNames grouped
  let matrix = buildMatrix allowedSourceNames grouped
  return matrix