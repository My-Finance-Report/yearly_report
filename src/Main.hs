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
        -- Compute bank flows (Income to other bank categories excluding Credit Card Payments)
        bankFlows = case Map.lookup "Income" bankAggregated of
            Just incomeTransactions ->
                let otherBankCategories = Map.filterWithKey (\k _ -> k /= "Income" && k /= "Credit Card Payments") bankAggregated
                    bankCategoryTotals = Map.map (sum . Prelude.map (signedAmount . transaction)) otherBankCategories
                in Prelude.map (\(category, total) -> ("Income", category, abs total)) (Map.toList bankCategoryTotals)
            Nothing -> []

        -- Compute the total flow from Income to Credit Card Payments
        creditCardPaymentsFromBank = case Map.lookup "Credit Card Payments" bankAggregated of
            Just ccTransactions -> sum $ Prelude.map (signedAmount . transaction) ccTransactions
            Nothing -> 0

        incomeToCC = [("Income", "Credit Card Payments", abs creditCardPaymentsFromBank) | creditCardPaymentsFromBank /= 0]

        -- Compute flows from Credit Card Payments to individual credit card categories
        ccFlowsToCategories = Prelude.map (\(category, transactions) ->
            ("Credit Card Payments", category, abs $ sum $ Prelude.map (signedAmount . transaction) transactions))
            (Map.toList ccAggregated)

    in bankFlows ++ incomeToCC ++ ccFlowsToCategories

-- Utility function to calculate signed amounts
signedAmount :: Transaction -> Double
signedAmount txn = case kind txn of
    Deposit    -> amount txn
    Withdrawal -> negate (amount txn)



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


