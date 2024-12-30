{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Bank
import CreditCard
import Categorizer
import Database
import HtmlGenerators

import Parsers

getAmountFromBank:: CategorizedTransaction BankRecord -> Double
getAmountFromBank txn =
        case Bank.transaction (Categorizer.transaction txn) of
            Deposit amt   -> amt
            Withdrawl amt -> amt


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


    let bankSummaryRows = generateAggregateRows aggregatedBankTransactions  getAmountFromBank
    let ccSummaryRows = generateAggregateRows aggregatedCCTransactions  (amount . Categorizer.transaction )

    let bankRows = generateBankHtml categorizedBankTransactions
    let creditCardRows = generateCreditCardHtml categorizedCCTransactions
    let fullSummary = generateHtml bankSummaryRows ccSummaryRows bankRows creditCardRows 

    TIO.writeFile "expense_summary.html" fullSummary
    putStrLn "Expense summary generated: expense_summary.html"




