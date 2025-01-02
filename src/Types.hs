{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
   ( Transaction(..)
    , CategorizedTransaction(..)
    , AggregatedTransactions
    , TransactionKind(..)
    , TransactionSource(..)
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

data TransactionSource = BankSource | CreditCardSource
    deriving (Eq, Show, Ord)

data TransactionKind = Withdrawal | Deposit
    deriving (Eq, Show, Ord, Generic)
  
instance FromJSON TransactionKind

data Transaction = Transaction
  { transactionDate :: Day
  , description :: Text
  , amount :: Double
  , kind :: TransactionKind
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Transaction where
    parseJSON = withObject "Transaction" $ \v -> do
        dateText <- v .: "transactionDate"
        let parsedDate = parseTimeM True defaultTimeLocale "%m/%d/%Y" (T.unpack dateText) :: Maybe Day
        case parsedDate of
            Just date -> Transaction
                <$> pure date
                <*> v .: "description"
                <*> v .: "amount"
                <*> v .: "kind"
            Nothing -> fail $ "Could not parse transactionDate: " <> T.unpack dateText



data CategorizedTransaction  = CategorizedTransaction
   { transaction :: Transaction
   , category :: Text
   , transactionSource :: TransactionSource
   } deriving (Show, Eq, Ord)

type AggregatedTransactions = Map.Map Text [CategorizedTransaction]