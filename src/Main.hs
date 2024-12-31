{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO

import Types
import Categorizer
import Database
import HtmlGenerators
import Parsers

import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
    let dbPath = "transactions.db"


    let bankDir = "bank_files"
    let ccDir = "credit_card_files"

    bankFiles <- map (bankDir </>) <$> listDirectory bankDir
    ccFiles <- map (ccDir </>) <$> listDirectory ccDir

    let ccCategories = ["Groceries", "Travel","Gas", "Misc", "Subscriptions", "Food"]
    let bankCategories = ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]

    initializeDatabase dbPath

    categorizedBankTransactions <- concat <$> mapM (\file -> processPdfFile dbPath file BankKind bankCategories) bankFiles
    categorizedCCTransactions <- concat <$> mapM (\file -> processPdfFile dbPath file CreditCardKind ccCategories) ccFiles

    let aggregatedBankTransactions = aggregateByCategory categorizedBankTransactions
    let aggregatedCCTransactions = aggregateByCategory categorizedCCTransactions

    let bankSummaryRows = generateAggregateRows aggregatedBankTransactions   
    let ccSummaryRows = generateAggregateRows aggregatedCCTransactions  

    let bankRows = generateTransactionTable categorizedBankTransactions
    let creditCardRows = generateTransactionTable categorizedCCTransactions
    let fullSummary = generateHtml bankSummaryRows ccSummaryRows bankRows creditCardRows 

    TIO.writeFile "expense_summary.html" fullSummary
    putStrLn "Expense summary generated: expense_summary.html"


