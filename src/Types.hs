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
    PdfParseException (..),
    CategorizationResponse (..),
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
    transactionSourceId :: Int
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
    category :: Text,
    transactionSource :: TransactionSource
  }
  deriving (Show, Eq, Ord)

type AggregatedTransactions = Map.Map Text [CategorizedTransaction]
