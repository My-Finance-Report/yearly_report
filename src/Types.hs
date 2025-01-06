{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Transaction (..),
    CategorizedTransaction (..),
    Category (..),
    AggregatedTransactions,
    TransactionKind (..),
    TransactionSource (..),
    TransactionsWrapper (..),
    SankeyConfig (..),
    PdfParseException (..),
    CategorizationResponse (..),
    UploadConfiguration (..),
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
import Database.SQLite.Simple.FromRow
import GHC.Generics (Generic)

newtype CategorizationResponse
  = CategorizationResponse {responseCategory :: Text}
  deriving (Show, Generic)

instance FromJSON CategorizationResponse

newtype PdfParseException
  = PdfParseException Text
  deriving (Show)

instance Exception PdfParseException

newtype TransactionsWrapper
  = TransactionsWrapper {transactions :: [Transaction]}
  deriving (Show, Generic)

instance FromJSON TransactionsWrapper

data TransactionSource = TransactionSource
  { sourceId :: Int,
    sourceName :: Text
  }
  deriving (Eq, Show, Ord, Generic)

instance FromJSON TransactionSource

data TransactionKind = Withdrawal | Deposit
  deriving (Eq, Show, Ord, Generic)

instance FromJSON TransactionKind

data Category = Category
  { categoryId :: Int,
    categoryName :: Text,
    transactionSource :: TransactionSource
  }
  deriving (Eq, Show, Ord, Generic)

instance FromJSON Category

data Transaction = Transaction
  { transactionDate :: Day,
    description :: Text,
    amount :: Double,
    kind :: TransactionKind
  }
  deriving (Show, Eq, Ord, Generic)

groupByBlah :: (Ord t) => (CategorizedTransaction -> t) -> [CategorizedTransaction] -> Map.Map t [CategorizedTransaction]
groupByBlah groupingFunc transactions =
  Map.fromListWith (++) [(groupingFunc txn, [txn]) | txn <- transactions]

groupByBlahForAll ::
  (Ord t) =>
  Map.Map TransactionSource [CategorizedTransaction] ->
  (CategorizedTransaction -> t) ->
  Map.Map TransactionSource (Map.Map t [CategorizedTransaction])
groupByBlahForAll groupedBySource groupingFunc =
  Map.map (groupingFunc `groupByBlah`) groupedBySource

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \v -> do
    dateText <- v .: "transactionDate"
    let parsedDate = parseTimeM True defaultTimeLocale "%m/%d/%Y" (T.unpack dateText) :: Maybe Day
    case parsedDate of
      Just date ->
        Transaction
          <$> pure date
          <*> v .: "description"
          <*> v .: "amount"
          <*> v .: "kind"
      Nothing -> fail $ "Could not parse transactionDate: " <> T.unpack dateText

data CategorizedTransaction = CategorizedTransaction
  { transaction :: Transaction,
    transactionId :: Maybe Int,
    category :: Category
  }
  deriving (Show, Eq, Ord)

type AggregatedTransactions = Map.Map Text [CategorizedTransaction]

-- FromRow Instances
instance FromRow TransactionSource where
  fromRow = TransactionSource <$> field <*> field

instance FromRow Category where
  fromRow =
    Category
      <$> field
      <*> field
      <*> (TransactionSource <$> field <*> field)

data UploadConfiguration = UploadConfiguration
  { startKeyword :: Text,
    endKeyword :: Text,
    transactionSourceId :: Int,
    filenameRegex :: Text
  }

instance FromRow UploadConfiguration where
  fromRow = UploadConfiguration <$> field <*> field <*> field <*> field

data SankeyConfig = SankeyConfig
  { inputs :: [(TransactionSource, Category)],
    linkages :: (TransactionSource, Category, TransactionSource),
    mapKeyFunction :: TransactionSource -> Text,
    configName :: Text
  }