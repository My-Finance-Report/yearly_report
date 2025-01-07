{-# LANGUAGE DeriveGeneric #-}

module Types
  ( CategorizedTransaction (..),
    AggregatedTransactions,
    TransactionKind (..),
    TransactionsWrapper (..),
    FullSankeyConfig (..),
    PdfParseException (..),
    CategorizationResponse (..),
    PartialTransaction (..),
    groupByBlah,
    groupByBlahForAll,
  )
where

import Control.Exception
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import Database.Persist
import Database.SQLite.Simple.FromRow
import GHC.Generics (Generic)
import Models

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
    partialTransactionDateOfTransaction :: Text,
    partialTransactionDescription :: Text,
    partialTransactionKind :: Text
  }
  deriving (Show, Generic)

instance FromJSON PartialTransaction

newtype TransactionsWrapper
  = TransactionsWrapper {transactions :: [PartialTransaction]}
  deriving (Show, Generic)

instance FromJSON TransactionKind

instance FromJSON Transaction

instance FromJSON TransactionsWrapper

groupByBlah ::
  (Ord t) =>
  (CategorizedTransaction -> t) ->
  [CategorizedTransaction] ->
  Map.Map t [CategorizedTransaction]
groupByBlah groupingFunc transactions =
  Map.fromListWith (++) [(groupingFunc txn, [txn]) | txn <- transactions]

groupByBlahForAll ::
  (Ord t) =>
  Map.Map (Entity TransactionSource) [CategorizedTransaction] ->
  (CategorizedTransaction -> t) ->
  Map.Map (Entity TransactionSource) (Map.Map t [CategorizedTransaction])
groupByBlahForAll groupedBySource groupingFunc =
  Map.map (groupingFunc `groupByBlah`) groupedBySource

data CategorizedTransaction = CategorizedTransaction
  { transaction :: Transaction,
    transactionId :: Maybe TransactionId,
    category :: Category
  }
  deriving (Show, Eq, Ord)

type AggregatedTransactions = Map.Map Text [CategorizedTransaction]

data FullSankeyConfig = FullSankeyConfig
  { inputs :: [(Entity TransactionSource, Entity Category)],
    linkages :: (Entity TransactionSource, Entity Category, Entity TransactionSource),
    mapKeyFunction :: Entity TransactionSource -> Text,
    configName :: Text
  }