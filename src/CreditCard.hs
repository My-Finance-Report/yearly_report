{-# LANGUAGE DeriveGeneric #-}

module CreditCard
   ( Transaction(..)
    , CategorizedTransaction(..)
    , AggregatedTransactions
    , TransactionKind(..)
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

data TransactionKind = BankKind | CreditCardKind
    deriving (Eq, Show, Ord)

-- TODO this should be moved into its own module, ,not CC specific
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