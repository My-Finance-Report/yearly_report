{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators (
    generateCreditCardHtml
    , generateBankHtml
    , generateHtml)
    where



import qualified Data.Map as Map
import Data.Text 
import Data.List (sortBy)
import Data.Ord (comparing)
import Categorizer (CategorizedTransaction (CategorizedTransaction), CategorizedCreditCardTransaction, category, transaction)
import Bank (CategorySummary)
import CreditCard

generateCreditCardRow :: CategorizedCreditCardTransaction -> Text
generateCreditCardRow categorizedTransaction =
    let txnCategory = category categorizedTransaction
        blahTransaction = transaction categorizedTransaction
        merchant = merchantName blahTransaction
        amount = CreditCard.amount blahTransaction
    in "<tr><td>" <> txnCategory <> "</td><td>" <> merchant <> "</td><td>" <> pack (show amount) <> "</td></tr>\n"


generateCreditCardHtml :: [CategorizedCreditCardTransaction] -> Text
generateCreditCardHtml categorizedTransactions =
    --     let tableRows = foldr () "" iterable ->>>> callable, starting value, iterable 
    let 
        sortedTransactions = sortBy (comparing category) categorizedTransactions
        tableRows = Prelude.foldr (\txn acc-> generateCreditCardRow txn <> acc) "" sortedTransactions
    in "<table>\n<tr><th>Category</th><th>Transactions</th> <th>Amount</th></tr>\n" <> tableRows <> "</table>\n"



generateBankHtml :: CategorySummary  -> Text
generateBankHtml summary =
    let tableRows = Map.foldrWithKey (\category total acc ->
            acc <> "<tr><td>" <> category <> "</td><td>" <> pack (show total) <> "</td></tr>\n"
            ) "" summary
    in "<table>\n "  <>  "<tr><th>Category</th><th>Total</th></tr>\n" <> tableRows <> "</table>\n"
    


generateHtml :: Text -> Text -> Text
generateHtml bank_summary credit_card_summary =
  "<!DOCTYPE html>\n<html>\n<head>\n<title>Expense Summary</title>\n</head>\n<body>\n" <> -- <> is a syntax for string concat
     "<h1>Expense Summary</h1>\n" <>
     bank_summary <> 
     credit_card_summary <>
     "</body>\n</html>"