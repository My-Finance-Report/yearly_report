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
import Data.Map
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Map as Map


generateSankeyData :: AggregatedTransactions -> AggregatedTransactions -> [(Text, Text, Double)]
generateSankeyData bankAggregated ccAggregated =
    let 
        calculateNetAmount :: [CategorizedTransaction] -> Double
        calculateNetAmount transactions =
            sum $ Prelude.map (\txn ->
                let amt = amount (transaction txn)
                    txnKind = kind (transaction txn)
                in case txnKind of
                    Deposit    -> amt
                    Withdrawal -> -amt
            ) transactions

        bankFlows = case Map.lookup "Income" bankAggregated of
            Just incomeTransactions ->
                let otherBankCategories = Map.filterWithKey (\k _ -> k /= "Income" && k /= "Credit Card Payments") bankAggregated
                    bankCategoryTotals = Map.map calculateNetAmount otherBankCategories
                in Prelude.map (\(category, total) -> ("Income", category, total)) (Map.toList bankCategoryTotals)
            Nothing -> []

        creditCardPaymentsFromBank = case Map.lookup "Credit Card Payments" bankAggregated of
            Just ccTransactions -> calculateNetAmount ccTransactions
            Nothing -> 0

        incomeToCC = [("Income", "Credit Card Payments", creditCardPaymentsFromBank) | creditCardPaymentsFromBank > 0]

        ccFlowsToCategories = Prelude.map (\(category, transactions) ->
            ("Credit Card Payments", category, calculateNetAmount transactions))
            (Map.toList ccAggregated)

    in bankFlows ++ incomeToCC ++ ccFlowsToCategories



main :: IO ()
main = do
    let dbPath = "transactions.db"


    let bankDir = "bank_files"
    let ccDir = "credit_card_files"

    bankFiles <- Prelude.map (bankDir </>) <$> listDirectory bankDir
    ccFiles <- Prelude.map (ccDir </>) <$> listDirectory ccDir

    let ccCategories = ["Groceries", "Travel","Gas", "Misc", "Subscriptions", "Food"]
    let bankCategories = ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]

    initializeDatabase dbPath

    categorizedBankTransactions <- concat <$> mapM (\file -> processPdfFile dbPath file BankSource bankCategories) bankFiles
    categorizedCCTransactions <- concat <$> mapM (\file -> processPdfFile dbPath file CreditCardSource ccCategories) ccFiles

    let aggregatedBankTransactions = aggregateByCategory categorizedBankTransactions
    let aggregatedCCTransactions = aggregateByCategory categorizedCCTransactions

    let aggregatedBankTransactionsMonth = aggregateByMonth categorizedBankTransactions
    let aggregatedCCTransactionsMonth = aggregateByMonth categorizedCCTransactions

    let bankSummaryRows = generateAggregateRows aggregatedBankTransactions   
    let ccSummaryRows = generateAggregateRows aggregatedCCTransactions  

    let bankMonthRows = generateAggregateRows aggregatedBankTransactionsMonth
    let ccMonthRows = generateAggregateRows aggregatedCCTransactionsMonth 

    let bankRows = generateTransactionTable categorizedBankTransactions
    let creditCardRows = generateTransactionTable categorizedCCTransactions

    let sankeyData = generateSankeyData aggregatedBankTransactions aggregatedCCTransactions
    print sankeyData
    let fullSummary = generateHtml bankSummaryRows ccSummaryRows bankRows creditCardRows  bankMonthRows ccMonthRows  sankeyData

    TIO.writeFile "expense_summary.html" fullSummary
    putStrLn "Expense summary generated: expense_summary.html"


