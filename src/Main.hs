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

import Parsers



type AggregatedTransactions t = Map.Map Text [t]

aggregateByCategory :: [CategorizedTransaction t] -> AggregatedTransactions t
aggregateByCategory = foldr insertTransaction Map.empty
  where
    insertTransaction :: CategorizedTransaction t -> AggregatedTransactions t -> AggregatedTransactions t
    insertTransaction categorizedTransaction acc =
        let cat = category categorizedTransaction
            txn = Categorizer.transaction categorizedTransaction
        in Map.insertWith (++) cat [txn] acc


main :: IO ()
main = do

    let dbPath = "transactions.db"
    let categories = ["Groceries", "Travel","Gas", "Misc", "Subscriptions", "Food"]
    let bankCategories = ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]
    
    initializeDatabase dbPath

    bankTransactions <- parseBankFile "bank_statement.csv"
    creditCardTransations <- parseCreditCardFile "credit_card.csv"


    categorizedBankTransactions <- mapM (\txn -> categorizeBankTransaction txn dbPath bankCategories) bankTransactions 
    categorizedCCTransactions <- mapM (\txn -> categorizeCreditCardTransaction txn dbPath categories) creditCardTransations


    let aggregatedBankTransactions = aggregateByCategory categorizedBankTransactions
    let aggregatedCCTransactions = aggregateByCategory categorizedCCTransactions


    let bankRows = generateBankHtml categorizedBankTransactions
    let creditCardRows = generateCreditCardHtml categorizedCCTransactions
    let fullSummary = generateHtml bankRows creditCardRows

    TIO.writeFile "expense_summary.html" fullSummary
    putStrLn "Expense summary generated: expense_summary.html"


    

    