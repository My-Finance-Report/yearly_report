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



main :: IO ()
main = do

    let dbPath = "transactions.db"
    let categories = ["Groceries", "Travel","Gas", "Misc", "Subscriptions", "Food"]
    
    initializeDatabase dbPath

    bankTransactions <- parseBankFile "bank_statement.csv"
    creditCardTransations <- parseCreditCardFile "credit_card.csv"

    let bankRows = generateBankHtml . aggregateByCategory $ bankTransactions

    categorizedTransactions <- mapM (\txn -> categorizeCreditCardTransaction txn dbPath categories) creditCardTransations

    let creditCardRows = generateCreditCardHtml categorizedTransactions

    let fullSummary = generateHtml bankRows creditCardRows

    TIO.writeFile "expense_summary.html" fullSummary
    putStrLn "Expense summary generated: expense_summary.html"


    

    