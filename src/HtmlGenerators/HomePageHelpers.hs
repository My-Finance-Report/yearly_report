{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HomePageHelpers
  ( formatCurrency,
    formatSankeyRow,
    applyGroupingLevels,
    groupByYearDescending,
    groupByMonthDescending,
    formatMonthYear,
    formatYear,
    parseMonthYear,
    formatFullDate,
    computeBalance,
    computeWithdrawals,
    computeGroupTotals,
    computeDeposits,
    computeTotals,
    subtabMappings,
    generateHeader,
    generateHistogramDiv,
    makeCharts,
    loadScripts,
  )
where

import Data.List (groupBy, head, null, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text as T hiding (concatMap, elem)
import Data.Time
import Database.Models
import Database.Persist
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
import Types

-- | Formats currency as "$X.XX"
formatCurrency :: Double -> Text
formatCurrency x
  | x < 0 = "-$" `append` pack (printf "%.2f" (abs x))
  | otherwise = "$" `append` pack (printf "%.2f" x)

-- | Formats a row for the Sankey diagram
formatSankeyRow :: (Text, Text, Double) -> Text
formatSankeyRow (from, to, weight) =
  "['" <> from <> "', '" <> to <> "', " <> T.pack (show weight) <> "],\n"

-- | Type alias for grouping function

-- | Defines available grouping methods for transactions
subtabMappings :: [(Text, [GroupingFunction])]
subtabMappings =
  [ --("Category", [groupByCategory]),
    ("Category → Month", [groupByCategory, groupByMonthDescending])
    --("Month", [groupByMonthDescending]),
    --("Year", [groupByYearDescending]),
    --("Year → Month → Category", [groupByYearDescending, groupByMonthDescending, groupByCategory])
  ]

applyGroupingLevels :: [CategorizedTransaction] -> [GroupingFunction] -> GroupedTransactions
applyGroupingLevels txns [] = Leaf txns
applyGroupingLevels txns (grpFunc : rest) =
  Node $ Map.map (`applyGroupingLevels` rest) (grpFunc txns)

groupByCategory :: GroupingFunction
groupByCategory transactions = do
  groupByBlah (categoryName . entityVal . category) transactions

-- | Groups transactions by Year in descending order
groupByYearDescending :: GroupingFunction
groupByYearDescending transactions =
  let grouped = groupByBlah (formatYear . transactionDateOfTransaction . entityVal . transaction) transactions
   in Map.fromList $ sortOn fst (Map.toList grouped)

-- | Groups transactions by Month in descending order
groupByMonthDescending :: GroupingFunction
groupByMonthDescending transactions =
  let grouped = groupByBlah (formatMonthYear . transactionDateOfTransaction . entityVal . transaction) transactions
   in Map.fromList $ sortOn (Down . parseMonthYear . fst) (Map.toList grouped)

-- | Formats a date as "Month Year" (e.g., "January 2023")
formatMonthYear :: UTCTime -> Text
formatMonthYear utcTime = T.pack (formatTime defaultTimeLocale "%B %Y" utcTime)

-- | Formats a date as "YYYY"
formatYear :: UTCTime -> Text
formatYear utcTime = T.pack (formatTime defaultTimeLocale "%Y" utcTime)

-- | Parses a "Month Year" string into UTCTime
parseMonthYear :: Text -> UTCTime
parseMonthYear text =
  fromMaybe (error "Invalid date format") $
    parseTimeM True defaultTimeLocale "%B %Y" (T.unpack text)

-- | Formats a full date as "MM/DD/YYYY"
formatFullDate :: UTCTime -> Text
formatFullDate utcTime = T.pack (formatTime defaultTimeLocale "%m/%d/%Y" utcTime)

-- | Computes the balance impact of a transaction
computeBalance :: Transaction -> Double
computeBalance txn =
  case transactionKind txn of
    Deposit -> transactionAmount txn
    Withdrawal -> negate $ transactionAmount txn

-- | Computes withdrawals from a transaction
computeWithdrawals :: Transaction -> Double
computeWithdrawals txn =
  case transactionKind txn of
    Deposit -> 0
    Withdrawal -> transactionAmount txn

-- | Computes deposits from a transaction
computeDeposits :: Transaction -> Double
computeDeposits txn =
  case transactionKind txn of
    Deposit -> transactionAmount txn
    Withdrawal -> 0

computeGroupTotals :: [CategorizedTransaction] -> (Text, Text, Text)
computeGroupTotals txns =
  ( formatCurrency $ sum $ Prelude.map (computeBalance . entityVal . transaction) txns,
    formatCurrency $ sum $ Prelude.map (computeWithdrawals . entityVal . transaction) txns,
    formatCurrency $ sum $ Prelude.map (computeDeposits . entityVal . transaction) txns
  )

computeTotals :: Map.Map Text [CategorizedTransaction] -> (Text, Text, Text)
computeTotals aggregated =
  let sumTransactions f =
        formatCurrency $
          sum [f (entityVal (transaction txn)) | txns <- Map.elems aggregated, txn <- txns]
   in ( sumTransactions computeBalance,
        sumTransactions computeWithdrawals,
        sumTransactions computeDeposits
      )

loadScripts = do
  H.script
    ! A.type_ "text/javascript"
    ! A.src "https://www.gstatic.com/charts/loader.js"
    $ mempty
  H.script
    ! A.type_ "text/javascript"
    ! A.src "/sankey.js"
    $ mempty
  H.script
    ! A.type_ "text/javascript"
    ! A.src "/tabs.js"
    $ mempty
  H.script
    ! A.type_ "text/javascript"
    ! A.src "/toggle.js"
    $ mempty
  H.script
    ! A.type_ "text/javascript"
    ! A.src "/histogram.js"
    $ mempty

generateHistogramDiv :: Html
generateHistogramDiv =
  H.div ! A.id "histogram_chart" $ mempty

generateHeader :: Html
generateHeader =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Expense Summary"

      H.script ! A.type_ "text/javascript" ! A.src "https://www.gstatic.com/charts/loader.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "/sankey.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "/tabs.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "/histogram.js" $ mempty

makeCharts :: Html
makeCharts =
  H.div ! A.class_ "flex flex-col md:flex-row gap-6 rounded-md p-4" $ do
    -- Sankey Chart
    H.div ! A.class_ "flex-1 border border-primary rounded-md p-4 shadow-md min-h-[300px] md:min-h-[500px] w-full overflow-hidden" $ do
      H.div
        ! A.id "sankeyChart"
        ! A.class_ "sankey-chart w-full h-full"
        $ ""

    -- Histogram Chart
    H.div ! A.class_ "flex-1 border border-primary rounded-md p-4 shadow-md min-h-[300px] md:min-h-[500px] w-full overflow-hidden" $ do
      H.div
        ! A.id "histogram_chart"
        ! A.class_ "histogram-chart w-full h-full"
        $ ""
