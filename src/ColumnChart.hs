{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ColumnChart (generateColChartData) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson hiding (Key)
import Data.Map (Map, elems, findWithDefault, fromListWith, keys, keysSet, singleton, toList, unionWith)
import qualified Data.Map
import Data.Set (Set, unions)
import qualified Data.Set
import Data.Text (Text, pack)
import Data.Time (UTCTime (..), defaultTimeLocale, formatTime, fromGregorian, parseTimeM, toGregorian)
import Database.Models
import Database.Persist
  ( Entity (entityKey, entityVal),
    PersistEntity (Key),
  )
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

generateColChartData :: (MonadUnliftIO m) => Entity User -> [CategorizedTransaction] -> m Matrix
generateColChartData user transactions = do
  sourceMap <- fetchSourceMap user
  let grouped = groupBySourceAndMonth sourceMap transactions
  let allSourceNames = extractAllSourceNames grouped
  let matrix = buildMatrix allSourceNames grouped
  return matrix

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

formatMonthYear :: UTCTime -> Text
formatMonthYear utcTime = pack (formatTime defaultTimeLocale "%b %Y" utcTime)
