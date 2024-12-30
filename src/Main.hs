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

import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeFileName)


processPdfFile :: FilePath -> FilePath -> IO [CreditCardTransaction]
processPdfFile dbPath pdfPath = do
    let filename = takeFileName pdfPath

    alreadyProcessed <- isFileProcessed dbPath (T.pack filename)
    if alreadyProcessed
        then do
            putStrLn $ "File '" ++ filename ++ "' has already been processed."
            return [] 
        else do
            putStrLn $ "Processing file: " ++ filename
            result <- try (extractTransactionsFromPdf pdfPath) :: IO (Either SomeException [CreditCardTransaction])
            case result of
                Left err -> do
                    putStrLn $ "Error processing file '" ++ filename ++ "': " ++ show err
                    return [] 
                Right transactions -> do
                    markFileAsProcessed dbPath (T.pack filename)
                    putStrLn $ "Extracted " ++ show (length transactions) ++ " transactions from '" ++ filename ++ "'."
                    return transactions 



getAmountFromBank:: CategorizedTransaction BankRecord -> Double
getAmountFromBank txn =
        case Bank.transaction (Categorizer.transaction txn) of
            Deposit amt   -> amt
            Withdrawl amt -> amt


main :: IO ()
main = do

    let dbPath = "transactions.db"
    let bankPath = "bank_statement.csv"
    let pdfPath= "20241219-VentureOne card statement-3996.pdf"
    let categories = ["Groceries", "Travel","Gas", "Misc", "Subscriptions", "Food"]
    let bankCategories = ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]

    initializeDatabase dbPath


    bankTransactions <- parseBankFile bankPath


    ccPDFtransactions <-processPdfFile dbPath pdfPath


    categorizedBankTransactions <- mapM (\txn -> categorizeBankTransaction txn dbPath bankCategories (T.pack bankPath)) bankTransactions
    categorizedCCTransactions <- mapM (\txn -> categorizeCreditCardTransaction txn dbPath categories (T.pack pdfPath)) ccPDFtransactions

    let aggregatedBankTransactions = aggregateByCategory categorizedBankTransactions
    let aggregatedCCTransactions = aggregateByCategory categorizedCCTransactions


    let bankSummaryRows = generateAggregateRows aggregatedBankTransactions  getAmountFromBank
    let ccSummaryRows = generateAggregateRows aggregatedCCTransactions  (amount . Categorizer.transaction )

    let bankRows = generateBankHtml categorizedBankTransactions
    let creditCardRows = generateCreditCardHtml categorizedCCTransactions
    let fullSummary = generateHtml bankSummaryRows ccSummaryRows bankRows creditCardRows 

    TIO.writeFile "expense_summary.html" fullSummary
    putStrLn "Expense summary generated: expense_summary.html"


