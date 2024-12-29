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

data CreditCardTransaction = CreditCardTransaction --  this is just convention to name these the same
  { transactionDate :: Day
  , merchantName :: Text
  , amount :: Double
  } deriving (Show, Eq, Ord) -- basically to allow things like mapping (Ord), printing (show), etc


type CreditCardSummary = Map.Map Text Double -- this is basically a type alias


parseTransaction :: Text -> Maybe CreditCardTransaction
parseTransaction line =
  -- could also write as a let ... in, then remove the where

  --let split_line = T.splitOn "," line
  --in 
  case split_line of
    -- this unpacks the list into a 5 member tuple (basically asserting a pattern)
    (dateStr:_:merchant:amountStr:_) -> do
      date <- parseTimeM True defaultTimeLocale "%b %d" (T.unpack dateStr)
      amount <- readMaybe (T.unpack $ T.dropWhile (== '$') amountStr)
      Just $ CreditCardTransaction date merchant amount
    -- if we dont have five lines then we fall through to here,which return Nothing
    _ -> Nothing
  -- i think the where syntax is more "functional" then let .. in 
  where
    split_line = T.splitOn "," line



-- Read transactions from a file
ingestTransactions :: FilePath -> IO [CreditCardTransaction]
ingestTransactions filePath = do
  content <- TIO.readFile filePath -- arrow for magic with IO

  -- the $ here is like the "lowest precedence" operator, not sure why we need it, maybe so we eval the MapMaybe before we return?
  return $ mapMaybe parseTransaction (T.lines content) -- map maybe drops the Nothings that come from parse


parseCreditCardFile ::  FilePath -> IO [CreditCardTransaction]
parseCreditCardFile filePath = do
  content <- TIO.readFile filePath -- IO syntax
  -- same $ for precendece as above
  return $ mapMaybe parseTransaction  (T.lines content )

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

