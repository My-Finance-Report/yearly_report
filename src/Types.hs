{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types
  ( CategorizedTransaction (..),
    AggregatedTransactions,
    PartialUploadConfig (..),
    TransactionKind (..),
    TransactionsWrapper (..),
    FullSankeyConfig (..),
    GroupingFunction,
    PdfParseException (..),
    CategorizationResponse (..),
    PartialTransaction (..),
    groupByBlah,
    groupByBlahForAll,
  )
where

import Control.Exception
import Data.Aeson (FromJSON (parseJSON), fromJSON, withObject, (.:))
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (Day, UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Database.Models
import Database.Persist
import GHC.Generics (Generic)

newtype CategorizationResponse
  = CategorizationResponse {responseCategory :: Text}
  deriving (Show, Generic)

instance FromJSON CategorizationResponse

newtype PdfParseException
  = PdfParseException Text
  deriving (Show)

instance Exception PdfParseException

data PartialTransaction = PartialTransaction
  { partialTransactionAmount :: Double,
    partialTransactionDateOfTransaction :: UTCTime,
    partialTransactionDescription :: Text,
    partialTransactionKind :: Text
  }
  deriving (Show, Generic)

instance FromJSON PartialTransaction where
  parseJSON = withObject "PartialTransaction" $ \v -> do
    amount <- v .: "partialTransactionAmount"
    dateText <- v .: "partialTransactionDateOfTransaction"
    description <- v .: "partialTransactionDescription"
    kind <- v .: "partialTransactionKind"
    parsedDate <- case parseTimeM True defaultTimeLocale "%m/%d/%Y" (T.unpack dateText) of
      Just d -> return d
      Nothing -> fail $ "Could not parse date: " <> T.unpack dateText
    return $ PartialTransaction amount parsedDate description kind

newtype TransactionsWrapper
  = TransactionsWrapper {transactions :: [PartialTransaction]}
  deriving (Show, Generic)

instance FromJSON TransactionKind

instance FromJSON Transaction

instance FromJSON TransactionsWrapper

data PartialUploadConfig
  = PartialUploadConfig {partialFilenameRegex :: Text, partialStartKeyword :: Text, partialEndKeyword :: Text}
  deriving (Show, Generic)

instance FromJSON PartialUploadConfig where
  parseJSON = withObject "PartialUploadConfig" $ \v -> do
    partialFilenameRegex <- v .: "fileIdKeyword"
    partialStartKeyword <- v .: "startKeyword"
    partialEndKeyword <- v .: "endKeyword"
    return PartialUploadConfig {..}

groupByBlah ::
  (Ord t) =>
  (CategorizedTransaction -> t) ->
  [CategorizedTransaction] ->
  Map.Map t [CategorizedTransaction]
groupByBlah groupingFunc transactions =
  Map.fromListWith (++) [(groupingFunc txn, [txn]) | txn <- transactions]

groupByBlahForAll ::
  (Ord t) =>
  Map.Map k [CategorizedTransaction] ->
  (CategorizedTransaction -> t) ->
  Map.Map k (Map.Map t [CategorizedTransaction])
groupByBlahForAll groupedBySource groupingFunc =
  Map.map (groupingFunc `groupByBlah`) groupedBySource

data CategorizedTransaction = CategorizedTransaction
  { transaction :: Entity Transaction,
    transactionId :: Maybe TransactionId, -- fine to deprecate now that this transaction is an entity
    category :: Entity Category
  }
  deriving (Show, Eq, Ord)

type AggregatedTransactions = Map.Map Text [CategorizedTransaction]

data FullSankeyConfig = FullSankeyConfig
  { inputs :: [(Entity TransactionSource, Entity Category)],
    linkages :: [(Entity TransactionSource, Entity Category, Entity TransactionSource)]
  }
  deriving (Show, Generic)

type GroupingFunction = [CategorizedTransaction] -> Map.Map Text [CategorizedTransaction]