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


-- TODO extract the existing rows from the DB if they have already been processed
processPdfFile :: FilePath -> FilePath -> IO [Transaction]
processPdfFile dbPath pdfPath = do
    let filename = takeFileName pdfPath

    alreadyProcessed <- isFileProcessed dbPath (T.pack filename)
    if alreadyProcessed
        then do
            putStrLn $ "File '" ++ filename ++ "' has already been processed."
            return [] 
        else do
            putStrLn $ "Processing file: " ++ filename
            result <- try (extractTransactionsFromPdf pdfPath) :: IO (Either SomeException [Transaction])
            case result of
                Left err -> do
                    putStrLn $ "Error processing file '" ++ filename ++ "': " ++ show err
                    return [] 
                Right transactions -> do
                    markFileAsProcessed dbPath (T.pack filename)
                    putStrLn $ "Extracted " ++ show (length transactions) ++ " transactions from '" ++ filename ++ "'."
                    return transactions 




main :: IO ()
main = do
    let dbPath = "transactions.db"
    let bankPath = "bank_statement.csv"
    let pdfPath= "20241219-VentureOne card statement-3996.pdf"

    let ccCategories = ["Groceries", "Travel","Gas", "Misc", "Subscriptions", "Food"]
    let bankCategories = ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]

    initializeDatabase dbPath

    bankTransactions <- parseBankFile bankPath
    ccPDFtransactions <-processPdfFile dbPath pdfPath


    categorizedBankTransactions <- mapM (\txn -> categorizeTransaction txn dbPath bankCategories (T.pack bankPath)) bankTransactions
    categorizedCCTransactions <- mapM (\txn -> categorizeTransaction txn dbPath ccCategories (T.pack pdfPath)) ccPDFtransactions

    let aggregatedBankTransactions = aggregateByCategory categorizedBankTransactions
    let aggregatedCCTransactions = aggregateByCategory categorizedCCTransactions

    let bankSummaryRows = generateAggregateRows aggregatedBankTransactions   
    let ccSummaryRows = generateAggregateRows aggregatedCCTransactions  

    let bankRows = generateTransactionTable categorizedBankTransactions
    let creditCardRows = generateTransactionTable categorizedCCTransactions
    let fullSummary = generateHtml bankSummaryRows ccSummaryRows bankRows creditCardRows 

    TIO.writeFile "expense_summary.html" fullSummary
    putStrLn "Expense summary generated: expense_summary.html"


