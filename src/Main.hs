{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Map as Map
import Data.Text (Text)

import qualified Data.Text.IO as TIO
import Bank
import CreditCard
import Categorizer
import Database

import HtmlGenerators


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


blah :: CreditCardTransaction -> FilePath -> [Text] -> IO CategorizedCreditCardTransaction
blah creditCardTransaction dbPath categories = do
    category <- categorizeTransaction dbPath (merchantName creditCardTransaction) categories
    return CategorizedCreditCardTransaction { Categorizer.transaction=creditCardTransaction , category=category}

main :: IO ()
main = do
    records <- parseBankFile "bank_statement.csv"
    let summary = aggregateByCategory records
        bankRows = generateBankHtml summary

    let dbPath = "transactions.db"
    initializeDatabase dbPath
    let categories = ["Groceries", "Travel","Gas", "Misc", "Subscriptions", "Food"]
    transactions <- ingestTransactions "credit_card.csv"

 -- Categorize transactions using mapM
    categorizedTransactions <- mapM (\txn -> blah txn dbPath categories) transactions

    -- Generate HTML rows for credit card transactions
    let creditCardRows = generateCreditCardHtml categorizedTransactions


    let fullSummary = generateHTML bankRows creditCardRows

    TIO.writeFile "expense_summary.html" fullSummary
    putStrLn "Expense summary generated: expense_summary.html"


    

    