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

generateRow :: CategorizedTransaction -> Text
generateRow categorizedTransaction =
    let txnCategory = category categorizedTransaction
        innerTransaction = transaction categorizedTransaction
        date = transactionDate innerTransaction
        description = Types.description innerTransaction
        amount = Types.amount innerTransaction
    in "<tr><td>" <> txnCategory <> "</td><td>" <> date <> "</td><td>" <> description <> "</td><td>" <> pack (show amount) <> "</td></tr>\n"



generateTransactionTable :: [CategorizedTransaction] -> Text
generateTransactionTable categorizedTransactions =
    --     let tableRows = foldr () "" iterable ->>>> callable, starting value, iterable 
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
    "<table>\n<tr><th>Description</th><th>Amount</th></tr>\n" <>
    T.concat (Prelude.map (\txn -> 
        "<tr><td>" <> (description . transaction) txn <> "</td><td>" <> pack (show (amount . transaction $ txn)) <> "</td></tr>\n") 
        transactions) <>
    "</table>\n</td>\n</tr>\n"

generateAggregateRows :: AggregatedTransactions -> Text
generateAggregateRows aggregatedTransactions =
    let tableRows = Map.foldrWithKey
                      (\category transactions acc ->
                          let totalAmount = sum (Prelude.map (amount . transaction) transactions)
                              sectionId = T.replace " " "-" category -- Unique ID for each category
                              categoryRow = generateAggregateRow category totalAmount sectionId
                              detailRows = generateDetailRows category transactions sectionId
                          in categoryRow <> detailRows <> acc
                      ) "" aggregatedTransactions
    in "<table>\n<tr><th>Category</th><th>Total Amount</th></tr>\n" <> tableRows <> "</table>\n"



generateHtml :: Text -> Text -> Text -> Text -> Text
generateHtml bankSummary creditCardSummary bankExpanded creditCardExpanded =
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
  "<h1>Credit Card Summary</h1>\n" <>
  creditCardSummary <>
  "<h1>Bank Expanded</h1>\n" <>
  bankExpanded <>
  "<h1>Credit Card Expanded</h1>\n" <>
  creditCardExpanded <>
  "</body>\n</html>"
