{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators (
    generateCreditCardHtml
    , generateBankHtml
    , generateAggregateRows
    , generateHtml)
    where


import qualified Data.Map as Map
import Data.Text 
import Data.List (sortBy)
import Data.Ord (comparing)
import Categorizer (AggregatedTransactions,CategorizedTransaction, CategorizedTransaction (transaction), CategorizedCreditCardTransaction, CategorizedBankTransaction, category, transaction)
import Bank (BankRecord (description, transaction ))
import CreditCard
import qualified Bank as BankRecord

generateCreditCardRow :: CategorizedCreditCardTransaction -> Text
generateCreditCardRow categorizedTransaction =
    let txnCategory = category categorizedTransaction
        blahTransaction = Categorizer.transaction categorizedTransaction
        merchant = CreditCard.description blahTransaction
        amount = CreditCard.amount blahTransaction
    in "<tr><td>" <> txnCategory <> "</td><td>" <> merchant <> "</td><td>" <> pack (show amount) <> "</td></tr>\n"

generateBankRow :: CategorizedBankTransaction -> Text
generateBankRow categorizedTransaction =
    let txnCategory = category categorizedTransaction
        blahTransaction = Categorizer.transaction categorizedTransaction
        merchant = Bank.description blahTransaction
        value = case BankRecord.transaction (Categorizer.transaction categorizedTransaction) of
                    BankRecord.Deposit amount   -> amount
                    BankRecord.Withdrawl amount -> amount
    in "<tr><td>" <> txnCategory <> "</td><td>" <> merchant <> "</td><td>" <> pack (show value) <> "</td></tr>\n"



generateCreditCardHtml :: [CategorizedCreditCardTransaction] -> Text
generateCreditCardHtml categorizedTransactions =
    --     let tableRows = foldr () "" iterable ->>>> callable, starting value, iterable 
    let 
        sortedTransactions = sortBy (comparing category) categorizedTransactions
        tableRows = Prelude.foldr (\txn acc-> generateCreditCardRow txn <> acc) "" sortedTransactions
    in "<table>\n<tr><th>Category</th><th>Transactions</th> <th>Amount</th></tr>\n" <> tableRows <> "</table>\n"



generateBankHtml :: [CategorizedBankTransaction]  -> Text
generateBankHtml categorizedTransactions =
    let 
        sortedTransactions = sortBy (comparing category) categorizedTransactions
        tableRows = Prelude.foldr (\txn acc-> generateBankRow txn <> acc) "" sortedTransactions
    in "<table>\n<tr><th>Category</th><th>Transactions</th> <th>Amount</th></tr>\n" <> tableRows <> "</table>\n"




generateAggregateRow :: Text -> Double -> Text
generateAggregateRow category amount = 
    "<tr><td>" <> category <> "</td><td>"  <> pack (show amount) <> "</td></tr>\n"

generateAggregateRows :: AggregatedTransactions a -> (CategorizedTransaction a -> Double) -> Text
generateAggregateRows aggregatedTransactions getAmount =
    let tableRows = Map.foldrWithKey
                      (\category transactions acc ->
                          let totalAmount = sum (Prelude.map getAmount transactions)
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