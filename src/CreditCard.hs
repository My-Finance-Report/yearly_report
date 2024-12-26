{-# LANGUAGE OverloadedStrings #-}

module CreditCard
   ( CreditCardTransaction(..)
  , ingestTransactions
  , groupTransactionsByMonth
  , groupTransactionsByMerchant
  , summarizeTransactions
  , parseCreditCardFile
  , generateCreditCardHtml
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
data CreditCardTransaction = CreditCardTransaction
  { transactionDate :: Day
  , merchantName :: Text
  , amount :: Double
  } deriving (Show, Eq, Ord)


type CreditCardSummary = Map.Map Text Double

parseTransaction :: Text -> Maybe CreditCardTransaction
parseTransaction line =
  case split_line of
    (dateStr:_:merchant:amountStr:_) -> do
      date <- parseTimeM True defaultTimeLocale "%b %d" (T.unpack dateStr)
      amount <- readMaybe (T.unpack $ T.dropWhile (== '$') amountStr)
      Just $ CreditCardTransaction date merchant amount
    _ -> Nothing
  where
    split_line = T.splitOn "," line



-- Read transactions from a file
ingestTransactions :: FilePath -> IO [CreditCardTransaction]
ingestTransactions filePath = do
  content <- TIO.readFile filePath
  return $ mapMaybe parseTransaction (T.lines content)


parseCreditCardFile ::  FilePath -> IO [CreditCardTransaction]
parseCreditCardFile filePath = do
  content <- TIO.readFile filePath
  let lines = T.lines content
  return $ mapMaybe parseTransaction lines

-- Helper to safely read a value
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing

-- Helper to format a date as "YYYY-MM"
formatMonth :: Day -> Text
formatMonth date = T.pack $ formatTime defaultTimeLocale "%Y-%m" date


-- Group transactions by their month
groupTransactionsByMonth :: [CreditCardTransaction] -> Map Text [CreditCardTransaction]
groupTransactionsByMonth transactions =
  Map.fromListWith (++) [(formatMonth (transactionDate t), [t]) | t <- transactions]


-- Group transactions by merchant
groupTransactionsByMerchant :: [CreditCardTransaction] -> Map Text [CreditCardTransaction]
groupTransactionsByMerchant transactions =
  Map.fromListWith (++) [(merchantName t, [t]) | t <- transactions]


-- Summarize transactions by total amount
summarizeTransactions :: Map Text [CreditCardTransaction] -> Map Text Double
summarizeTransactions = Map.map (sum . map amount)


generateCreditCardHtml :: CreditCardSummary -> Text
generateCreditCardHtml summary =
    let tableRows = Map.foldrWithKey (\category total acc ->
            acc <> "<tr><td>" <> category <> "</td><td>" <> T.pack (show total) <> "</td></tr>\n"
            ) "" summary
    in "<table>\n" <> tableRows <> "</table>\n"

