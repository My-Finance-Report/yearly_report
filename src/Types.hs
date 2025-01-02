{-# LANGUAGE DeriveGeneric #-}

module Types
   ( Transaction(..)
    , CategorizedTransaction(..)
    , AggregatedTransactions
    , TransactionKind(..)
    , TransactionsWrapper(..)
    , PdfParseException(..)
    , CategorizationResponse(..)
  ) where

import Data.Time (Day, parseTimeM, defaultTimeLocale, formatTime)
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import GHC.Generics (Generic)
import Control.Exception


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

data TransactionKind = BankKind | CreditCardKind
    deriving (Eq, Show, Ord)

data Transaction = Transaction
  { transactionDate :: Day
  , description :: Text
  , amount :: Double
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Transaction

data CategorizedTransaction  = CategorizedTransaction
   { transaction :: Transaction
   , category :: Text
   , transactionKind :: TransactionKind
   } deriving (Show, Eq, Ord)

type AggregatedTransactions = Map.Map Text [CategorizedTransaction]