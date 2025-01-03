{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO

import Types
import Categorizer
import Database
import HtmlGenerators
import Parsers

import Control.Exception (try, SomeException)
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Data.Map
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Map as Map
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as Web
import Network.HTTP.Client (Request(redactHeaders))


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

mainInner::  IO TL.LazyText
mainInner = do
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
    let strictText = generateHtml bankSummaryRows ccSummaryRows bankRows creditCardRows  bankMonthRows ccMonthRows  sankeyData
    return  $ TL.fromStrict strictText



main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev

    get "/" $ do
        content <- liftIO mainInner
        Web.html content

    post "/upload" $ do
        liftIO $ putStrLn "Received a POST to /upload"
        Web.text "Thanks for uploading!"

    get "/transactions" $ do
        let dbPath = "transactions.db"
        filenames <- liftIO $ getAllFilenames dbPath
        Web.html $ renderAllFilesPage filenames

    get "/transactions/:filename" $ do

      let dbPath = "transactions.db"
      filename <- Web.Scotty.pathParam "filename"
      liftIO $ initializeDatabase dbPath  
      transactions <- liftIO $ getAllTransactions dbPath filename
      Web.html $ renderTransactionsPage (T.pack filename) transactions

    post "/update-category" $ do
      let dbPath = "transactions.db"
      tId     <- Web.Scotty.formParam "transactionId"   :: ActionM T.Text
      newCat  <- Web.Scotty.formParam "newCategory"     :: ActionM T.Text
      fileArg <- Web.Scotty.formParam "filename"        :: ActionM T.Text
      liftIO $ updateTransactionCategory dbPath (read $ T.unpack tId) newCat
      redirect $ TL.fromStrict ("/transactions/" <> fileArg)
