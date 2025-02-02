{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ColumnChart (generateColChartData) where

import Control.Exception (Exception, throw, throwIO)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import Crypto.PubKey.RSA.Prim (dp)
import Data.Aeson hiding (Key)
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

truncateToMonth :: UTCTime -> UTCTime
truncateToMonth utcTime =
  let (year, month, _) = toGregorian (utctDay utcTime)
   in UTCTime (fromGregorian year month 1) 0

formatMonthYear :: UTCTime -> Text
formatMonthYear utcTime = pack (formatTime defaultTimeLocale "%b %Y" utcTime)

isWithdrawal :: CategorizedTransaction -> Bool
isWithdrawal txn =
  case transactionKind $ entityVal (transaction txn) of
    Deposit -> False
    Withdrawal -> True

getWithdrawalAmount :: CategorizedTransaction -> Double
getWithdrawalAmount txn =
  case transactionKind $ entityVal (transaction txn) of
    Deposit -> 0
    Withdrawal -> transactionAmount $ entityVal $ transaction txn

groupBySourceAndMonth ::
  Map (Key TransactionSource) TransactionSource ->
  [CategorizedTransaction] ->
  Map UTCTime (Map Text Double)
groupBySourceAndMonth sourceMap txns =
  fromListWith
    (unionWith (+))
    [ ( truncateToMonth $ transactionDateOfTransaction $ entityVal $ transaction txn,
        singleton (resolveSourceName sourceMap txn) (getWithdrawalAmount txn)
      )
      | txn <- txns,
        isWithdrawal txn
    ]

resolveSourceName ::
  Map (Key TransactionSource) TransactionSource ->
  CategorizedTransaction ->
  Text
resolveSourceName sourceMap txn =
  let sourceId = categorySourceId $ entityVal $ category txn
   in findWithDefault "Unknown" sourceId (Data.Map.map transactionSourceName sourceMap) <> " Withdrawals"

extractAllSourceNames :: Map UTCTime (Map Text Double) -> [Text]
extractAllSourceNames groupedData =
  Data.Set.toList $ unions (Prelude.map keysSet $ elems groupedData)

buildMatrix ::
  [Text] ->
  Map UTCTime (Map Text Double) ->
  Matrix
buildMatrix allSourceNames groupedData =
  let columnHeaders = "Month" : allSourceNames
      rowHeaders = Prelude.map formatMonthYear (keys groupedData)
      dataRows = Prelude.map (buildRow allSourceNames) (toList groupedData)
   in Matrix columnHeaders rowHeaders dataRows

buildRow ::
  [Text] ->
  (UTCTime, Map Text Double) ->
  [Double]
buildRow allSourceNames (_, sources) =
  Prelude.map (\source -> findWithDefault 0 source sources) allSourceNames

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

generateColChartData :: (MonadUnliftIO m) => Entity User -> [CategorizedTransaction] -> m Matrix
generateColChartData user transactions = do
  sourceMap <- fetchSourceMap user
  config <- selectDefaultConfig user
  allowedSources <- sourcesFromConfig user config
  let filteredSourceMap = filterByAllowedSources allowedSources sourceMap
  let grouped = groupBySourceAndMonth filteredSourceMap transactions
  let allowedSourceNames = extractAllSourceNames grouped
  let matrix = buildMatrix allowedSourceNames grouped
  return matrix