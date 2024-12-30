{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators (
    generateTransactionTable
    , generateAggregateRows
    , generateHtml)
    where


import qualified Data.Map as Map
import Data.Text 
import Data.List (sortBy)
import Data.Ord (comparing)
import Categorizer (AggregatedTransactions,CategorizedTransaction, CategorizedTransaction (transaction), category, transaction)
import CreditCard

-- TODO why is this called blah
generateRow :: CategorizedTransaction -> Text
generateRow categorizedTransaction =
    let txnCategory = category categorizedTransaction
        blahTransaction = Categorizer.transaction categorizedTransaction
        description = CreditCard.description blahTransaction
        amount = CreditCard.amount blahTransaction
    in "<tr><td>" <> txnCategory <> "</td><td>" <> description <> "</td><td>" <> pack (show amount) <> "</td></tr>\n"



generateTransactionTable :: [CategorizedTransaction] -> Text
generateTransactionTable categorizedTransactions =
    --     let tableRows = foldr () "" iterable ->>>> callable, starting value, iterable 
    let 
        sortedTransactions = sortBy (comparing category) categorizedTransactions
        tableRows = Prelude.foldr (\txn acc-> generateRow txn <> acc) "" sortedTransactions
    in "<table>\n<tr><th>Category</th><th>Transactions</th> <th>Amount</th></tr>\n" <> tableRows <> "</table>\n"



generateAggregateRow :: Text -> Double -> Text
generateAggregateRow category amount = 
    "<tr><td>" <> category <> "</td><td>"  <> pack (show amount) <> "</td></tr>\n"

generateAggregateRows :: AggregatedTransactions  -> Text
generateAggregateRows aggregatedTransactions  =
    let tableRows = Map.foldrWithKey
                      (\category transactions acc ->
                          let totalAmount = sum (Prelude.map (amount . Categorizer.transaction ) transactions)
                          in generateAggregateRow category totalAmount <> acc
                      ) "" aggregatedTransactions
    in "<table>\n<tr><th>Category</th><th>Total Amount</th></tr>\n" <> tableRows <> "</table>\n"
 


generateHtml::Text -> Text -> Text -> Text -> Text
generateHtml bank_summary credit_card_summary bank_expanded credit_card_expanded  =
  "<!DOCTYPE html>\n<html>\n<head>\n<title>Expense Summary</title>\n</head>\n<body>\n" <> -- <> is a syntax for string concat
     "<h1>Expense Summary</h1>\n" <>
     bank_summary <> 
     credit_card_summary <>
     bank_expanded <>
     credit_card_expanded <>
     "</body>\n</html>"