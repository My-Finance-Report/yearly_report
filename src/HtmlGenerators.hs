{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators (
    generateTransactionTable
    , generateAggregateRows
    , generateHtml)
    where


import qualified Data.Map as Map
import Data.Text as T
import Data.List (sortBy)
import Data.Ord (comparing)
import Types
import Data.Map
import Data.Time

truncateToTwoDecimals :: Double -> Double
truncateToTwoDecimals x = fromIntegral (truncate (x * 100)) / 100

prettyFormat :: Day -> Text
prettyFormat = T.pack . formatTime defaultTimeLocale "%B %Y"

generateRow :: CategorizedTransaction -> Text
generateRow categorizedTransaction =
    let txnCategory = category categorizedTransaction
        innerTransaction = transaction categorizedTransaction
        date = formatTime defaultTimeLocale "%B %Y" (transactionDate innerTransaction) 
        description = Types.description innerTransaction
        amount = truncateToTwoDecimals $ Types.amount innerTransaction
    in "<tr><td>" <> txnCategory <> "</td><td>" <> pack date <> "</td><td>" <> description <> "</td><td>" <> pack (show amount) <> "</td></tr>\n"


generateTransactionTable :: [CategorizedTransaction] -> Text
generateTransactionTable categorizedTransactions =
    let
        sortedTransactions = sortBy (comparing category) categorizedTransactions
        tableRows = Prelude.foldr (\txn acc-> generateRow txn <> acc) "" sortedTransactions
    in "<table>\n<tr><th>Category</th> <th>Date</th> <th>Transactions</th> <th>Amount</th></tr>\n" <> tableRows <> "</table>\n"


generateAggregateRow :: Text -> Double -> Text -> Text
generateAggregateRow category amount sectionId =
    "<tr class='expandable' onclick=\"toggleDetails('" <> sectionId <> "')\">\n" <>
    "<td>" <> category <> "</td><td>" <> pack (show amount) <> "</td>\n" <>
    "</tr>\n"

generateDetailRows :: Text -> [CategorizedTransaction] -> Text -> Text
generateDetailRows category transactions sectionId =
    "<tr id='" <> sectionId <> "' class='hidden'>\n" <>
    "<td colspan='2'>\n" <>
    "<table>\n<tr><th>Description</th> <th>Date </th> <th>Amount</th></tr>\n" <>
    T.concat (Prelude.map (\txn ->
        "<tr><td>" <> (description . transaction) txn <> "</td><td>" <> ( prettyFormat . transactionDate . transaction  ) txn <> "</td><td>"  <> pack (show (truncateToTwoDecimals . amount . transaction $ txn)) <> "</td></tr>\n")
        transactions) <>
    "</table>\n</td>\n</tr>\n"


generateAggregateRows :: AggregatedTransactions -> Text
generateAggregateRows aggregatedTransactions =
    let tableRows = Map.foldrWithKey
                      (\category transactions acc ->
                          let totalAmount =  truncateToTwoDecimals $ sum (Prelude.map (amount . transaction) transactions)
                              sectionId = T.replace " " "-" category
                              categoryRow = generateAggregateRow category totalAmount sectionId
                              detailRows = generateDetailRows category transactions sectionId
                          in categoryRow <> detailRows <> acc
                      ) "" aggregatedTransactions
    in "<table>\n<tr><th>Category</th> <th>Total Amount</th></tr>\n" <> tableRows <> "</table>\n"


generateHtml :: Text -> Text -> Text -> Text -> Text -> Text -> [(Text, Text, Double)] -> Text
generateHtml bankSummary creditCardSummary bankExpanded creditCardExpanded bankMonth creditCardMonth sankeyData =
  "<!DOCTYPE html>\n<html>\n<head>\n" <>
  "<title>Expense Summary</title>\n" <>
  "<style>\n" <>
  "body { font-family: Arial, sans-serif; line-height: 1.6; margin: 0; padding: 20px; background-color: #f4f4f9; }\n" <>
  "h1 { color: #333; }\n" <>
  "table { width: 100%; border-collapse: collapse; margin-bottom: 20px; }\n" <>
  "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }\n" <>
  "th { background-color: #f2f2f2; }\n" <>
  "tr:nth-child(even) { background-color: #f9f9f9; }\n" <>
  ".hidden { display: none; }\n" <>
  ".expandable { cursor: pointer; color: #007BFF; text-decoration: underline; }\n" <>
  "</style>\n" <>
  "<script type=\"text/javascript\" src=\"https://www.gstatic.com/charts/loader.js\"></script>\n" <>
  "<script type=\"text/javascript\">\n" <>
  "google.charts.load('current', {packages:['sankey']});\n" <>
  "google.charts.setOnLoadCallback(drawChart);\n" <>
  "function drawChart() {\n" <>
  "  const data = new google.visualization.DataTable();\n" <>
  "  data.addColumn('string', 'From');\n" <>
  "  data.addColumn('string', 'To');\n" <>
  "  data.addColumn('number', 'Weight');\n" <>
  "  data.addRows([\n" <>
  T.concat (Prelude.map formatSankeyRow sankeyData) <>
  "  ]);\n" <>
  "  const options = { width: 800, height: 600 };\n" <>
  "  const chart = new google.visualization.Sankey(document.getElementById('sankey_chart'));\n" <>
  "  chart.draw(data, options);\n" <>
  "}\n" <>
  "</script>\n" <>
  "<script>\n" <>
  "function toggleDetails(id) {\n" <>
  "  var element = document.getElementById(id);\n" <>
  "  if (element.classList.contains('hidden')) {\n" <>
  "    element.classList.remove('hidden');\n" <>
  "  } else {\n" <>
  "    element.classList.add('hidden');\n" <>
  "  }\n" <>
  "}\n" <>
  "</script>\n" <>
  "</head>\n<body>\n" <>
  "<h1>Bank Summary</h1>\n" <>
  bankSummary <>
  "<h1>Flow Diagram</h1>\n" <>
  "<div id=\"sankey_chart\"></div>\n" <>
  "<h1>Credit Card Summary</h1>\n" <>
  creditCardSummary <>
  "<h1>Bank By Month</h1>\n" <>
  bankMonth <>
  "<h1>Credit Card By Month</h1>\n" <>
  creditCardMonth <>
  "<h1>Bank Expanded</h1>\n" <>
  bankExpanded <>
  "<h1>Credit Card Expanded</h1>\n" <>
  creditCardExpanded <>
  "</body>\n</html>"

formatSankeyRow :: (Text, Text, Double) -> Text
formatSankeyRow (from, to, weight) =
  "['" <> from <> "', '" <> to <> "', " <> T.pack (show weight) <> "],\n"

