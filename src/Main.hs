{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO

import Bank
import CreditCard
import Categorizer
import Database
import HtmlGenerators
import Parsers

import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T




main :: IO ()
main = do
    let dbPath = "transactions.db"
    let bankPath = "bank_statement.csv"
    let pdfPath= "20241219-VentureOne card statement-3996.pdf"

    let ccCategories = ["Groceries", "Travel","Gas", "Misc", "Subscriptions", "Food"]
    let bankCategories = ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]

    initializeDatabase dbPath

    bankTransactions <- parseBankFile bankPath 
    ccPDFtransactions <-processPdfFile dbPath pdfPath CreditCardKind

    categorizedBankTransactions <- mapM (\txn -> categorizeTransaction txn dbPath bankCategories  bankPath BankKind) bankTransactions 
    categorizedCCTransactions <- mapM (\txn -> categorizeTransaction txn dbPath ccCategories  pdfPath CreditCardKind) ccPDFtransactions

    let aggregatedBankTransactions = aggregateByCategory categorizedBankTransactions
    let aggregatedCCTransactions = aggregateByCategory categorizedCCTransactions

    let bankSummaryRows = generateAggregateRows aggregatedBankTransactions   
    let ccSummaryRows = generateAggregateRows aggregatedCCTransactions  

    let bankRows = generateTransactionTable categorizedBankTransactions
    let creditCardRows = generateTransactionTable categorizedCCTransactions
    let fullSummary = generateHtml bankSummaryRows ccSummaryRows bankRows creditCardRows 

    TIO.writeFile "expense_summary.html" fullSummary
    putStrLn "Expense summary generated: expense_summary.html"


