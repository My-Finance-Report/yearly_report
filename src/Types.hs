{-# LANGUAGE DeriveGeneric #-}

module Types
   ( Transaction(..)
    , CategorizedTransaction(..)
    , AggregatedTransactions
    , TransactionKind(..)
    , TransactionsWrapper(..)
    , PdfParseException(..)
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
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Control.Exception

data PdfParseException = PdfParseException Text deriving (Show)

instance Exception PdfParseException

data TransactionsWrapper = TransactionsWrapper
  { transactions :: [Transaction]
  } deriving (Show, Generic)

instance FromJSON TransactionsWrapper



data TransactionKind = BankKind | CreditCardKind
    deriving (Eq, Show, Ord)

data Transaction = Transaction
  { transactionDate :: Text
  , description :: Text
  , amount :: Double
  } deriving (Show, Eq, Ord, Generic) -- basically to allow things like mapping (Ord), printing (show), etc

instance FromJSON Transaction

data CategorizedTransaction  = CategorizedTransaction
   { transaction :: Transaction
   , category :: Text
   , transactionKind :: TransactionKind
   } deriving (Show, Eq, Ord)

type AggregatedTransactions = Map.Map Text [CategorizedTransaction]