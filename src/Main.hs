{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Map as Map
import Data.Text (Text)

import qualified Data.Text.IO as TIO
import Bank
import CreditCard
import Categorizer
import Database


generateHTML :: Text -> Text -> Text
generateHTML bank_summary credit_card_summary =
  "<!DOCTYPE html>\n<html>\n<head>\n<title>Expense Summary</title>\n</head>\n<body>\n" <> -- <> is a syntax for string concat
     "<h1>Expense Summary</h1>\n" <>
     "<table border='1'>\n" <>
     "<tr><th>Category</th><th>Total</th></tr>\n" <>
     bank_summary <> 
     credit_card_summary <>
     "</table>\n" <>
     "</body>\n</html>"




main :: IO ()
main = do
    records <- parseBankFile "bank_statement.csv"
    let summary = aggregateByCategory records
        bankRows = generateBankHtml summary

    ccRecords <- ingestTransactions "credit_card.csv"
     -- likely superflous to group by transaction merchant
    let ccSummary =  summarizeTransactions ( groupTransactionsByMerchant ccRecords )
        creditCardRows = generateCreditCardHtml ccSummary


    let fullSummary = generateHTML bankRows creditCardRows

    print summary
    print ccSummary
    TIO.writeFile "expense_summary.html" fullSummary
    putStrLn "Expense summary generated: expense_summary.html"


    let dbPath = "transactions.db"
    initializeDatabase dbPath
    let categories = ["Groceries", "Travel","Gas", "Misc", "Subscriptions", "Food"]
    transactions <- ingestTransactions "credit_card.csv"

    categorizedTransactions <- mapM (\txn -> categorizeTransaction dbPath (merchantName txn) categories) transactions

    mapM_ print (zip transactions categorizedTransactions)

    