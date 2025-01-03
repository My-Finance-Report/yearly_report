{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HomePage (
    renderHomePage
) where


import qualified Data.Map as Map hiding ((!))
import Data.Text as T
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Map hiding ((!))
import Data.Time
import Control.Monad (forM_)
import qualified Data.Text.IO as TIO

import Categorizer
import Database
import HtmlGenerators.HtmlGenerators
import Parsers
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Control.Exception (try, SomeException)
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Map as Map
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Parse (FileInfo(..), tempFileBackEnd)
import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as Web
import Network.HTTP.Client (Request(redactHeaders))
import qualified Data.ByteString.Lazy as B
import Data.Text.Encoding (decodeUtf8)
import Types




renderHtmlT :: Html -> TL.Text
renderHtmlT = renderHtml

truncateToTwoDecimals :: Double -> Double
truncateToTwoDecimals x = fromIntegral (truncate (x * 100)) / 100

prettyFormat :: Day -> Text
prettyFormat = T.pack . formatTime defaultTimeLocale "%B %Y"


formatSankeyRow :: (Text, Text, Double) -> Text
formatSankeyRow (from, to, weight) =
  "['" <> from <> "', '" <> to <> "', " <> T.pack (show weight) <> "],\n"



generateRow :: CategorizedTransaction -> Html
generateRow categorizedTransaction = 
  let txnCategory       = category categorizedTransaction
      innerTransaction  = transaction categorizedTransaction
      dateStr           = formatTime defaultTimeLocale "%B %Y" (transactionDate innerTransaction)
      desc              = description innerTransaction
      txnKind           = T.pack (show (kind innerTransaction))
      amt               = truncateToTwoDecimals (amount innerTransaction)
  in H.tr $ do
       H.td (toHtml txnCategory)
       H.td (toHtml txnKind)
       H.td (toHtml dateStr)
       H.td (toHtml desc)
       H.td (toHtml (show amt))


generateTransactionTable :: [CategorizedTransaction] -> Html
generateTransactionTable categorizedTransactions =
  let sortedTransactions = sortBy (comparing category) categorizedTransactions
  in H.table $ do
       H.tr $ do
         H.th "Category"
         H.th "Kind"
         H.th "Date"
         H.th "Transactions"
         H.th "Amount"
       mapM_ generateRow sortedTransactions




generateAggregateRow :: T.Text -> Double -> T.Text -> Html
generateAggregateRow cat totalAmt sectionId = 
  H.tr ! A.class_ "expandable" ! A.onclick (H.toValue $ "toggleDetails('" <> sectionId <> "')") $ do
    H.td (toHtml cat)
    H.td (toHtml (show totalAmt))


generateDetailRows :: T.Text -> [CategorizedTransaction] -> T.Text -> Html
generateDetailRows cat txs sectionId = 
  H.tr ! A.id (H.toValue sectionId) ! A.class_ "hidden" $ do
    -- Make a nested table
    H.td ! A.colspan "2" $ do
      H.table $ do
        H.tr $ do
          H.th "Description"
          H.th "Date"
          H.th "Amount"
        mapM_ (detailRow . transaction) txs

  where
    detailRow :: Transaction -> Html
    detailRow t = 
      H.tr $ do
        H.td (toHtml (description t))
        H.td (toHtml (prettyFormat (transactionDate t)))
        H.td (toHtml (show (truncateToTwoDecimals (amount t))))




generateAggregateRows :: AggregatedTransactions -> Html
generateAggregateRows aggregatedTransactions =
  H.table $ do
    H.tr $ do
      H.th "Category"
      H.th "Total Amount"
    -- fold over the Map
    Map.foldrWithKey
      (\cat txs accHtml ->
         let totalAmount = truncateToTwoDecimals $
               sum (Prelude.map (\txn -> 
                  case kind (transaction txn) of
                    Deposit    -> amount (transaction txn)
                    Withdrawal -> - amount (transaction txn)
               ) txs)
             sectionId = T.replace " " "-" cat
         in do
              generateAggregateRow cat totalAmount sectionId
              generateDetailRows cat txs sectionId
              accHtml
      )
      (return ())   -- starting "Html" is 'return ()'
      aggregatedTransactions



generateSankeyData :: AggregatedTransactions -> AggregatedTransactions -> [(Text, Text, Double)]
generateSankeyData bankAggregated ccAggregated =
    let
        bankFlows = case Map.lookup "Income" bankAggregated of
            Just incomeTransactions ->
                let otherBankCategories = Map.filterWithKey (\k _ -> k /= "Income" && k /= "Credit Card Payments") bankAggregated
                    bankCategoryTotals = Map.map (sum . Prelude.map (signedAmount . transaction)) otherBankCategories
                in Prelude.map (\(category, total) -> ("Income", category, abs total)) (Map.toList bankCategoryTotals)
            Nothing -> []

        creditCardPaymentsFromBank = case Map.lookup "Credit Card Payments" bankAggregated of
            Just ccTransactions -> sum $ Prelude.map (signedAmount . transaction) ccTransactions
            Nothing -> 0

        incomeToCC = [("Income", "Credit Card Payments", abs creditCardPaymentsFromBank) | creditCardPaymentsFromBank /= 0]

        ccFlowsToCategories = Prelude.map (\(category, transactions) ->
            ("Credit Card Payments", category, abs $ sum $ Prelude.map (signedAmount . transaction) transactions))
            (Map.toList ccAggregated)

    in bankFlows ++ incomeToCC ++ ccFlowsToCategories

signedAmount :: Transaction -> Double
signedAmount txn = case kind txn of
    Deposit    -> amount txn
    Withdrawal -> negate (amount txn)




generateHtmlBlaze 
  :: Html
  -> Html
  -> Html
  -> Html
  -> Html
  -> Html
  -> [(T.Text, T.Text, Double)]    
  -> TL.Text                       
generateHtmlBlaze bankSummary creditCardSummary bankExpanded creditCardExpanded bankMonth creditCardMonth sankeyData =
  renderHtmlT $ do
    H.docTypeHtml $ do
      H.head $ do
        H.title "Expense Summary"

        H.link 
            ! A.rel "stylesheet"
            ! A.type_ "text/css"
            ! A.href "/style.css"


        H.script ! A.type_ "text/javascript" ! A.src "https://www.gstatic.com/charts/loader.js" $ mempty
        -- Inline script for Sankey
        H.script ! A.type_ "text/javascript" $ H.toHtml $ T.concat
          [ "google.charts.load('current', {packages:['sankey']});\n"
          , "google.charts.setOnLoadCallback(drawChart);\n"
          , "function drawChart() {\n"
          , "  const data = new google.visualization.DataTable();\n"
          , "  data.addColumn('string', 'From');\n"
          , "  data.addColumn('string', 'To');\n"
          , "  data.addColumn('number', 'Weight');\n"
          , "  data.addRows([\n"
          , T.concat (Prelude.map formatSankeyRow sankeyData)
          , "  ]);\n"
          , "  const options = { width: 800, height: 600 };\n"
          , "  const chart = new google.visualization.Sankey(document.getElementById('sankey_chart'));\n"
          , "  chart.draw(data, options);\n"
          , "}\n"
          ]
        -- Inline script for toggling details
        H.script $ H.toHtml $ T.concat
          [ "function toggleDetails(id) {\n"
          , "  var element = document.getElementById(id);\n"
          , "  if (element.classList.contains('hidden')) {\n"
          , "    element.classList.remove('hidden');\n"
          , "  } else {\n"
          , "    element.classList.add('hidden');\n"
          , "  }\n"
          , "}\n"
          ]
      H.body $ do
        H.h1 "Bank Summary"
        -- If bankSummary is already HTML, use "preEscapedToHtml" (be careful!)
        H.preEscapedToHtml bankSummary

        H.h1 "Flow Diagram"
        H.div ! A.id "sankey_chart" $ mempty

        H.h1 "Credit Card Summary"
        H.preEscapedToHtml creditCardSummary

        H.h1 "Bank By Month"
        H.preEscapedToHtml bankMonth

        H.h1 "Credit Card By Month"
        H.preEscapedToHtml creditCardMonth

        H.h1 "Bank Expanded"
        H.preEscapedToHtml bankExpanded

        H.h1 "Credit Card Expanded"
        H.preEscapedToHtml creditCardExpanded




renderHomePage::  IO TL.LazyText
renderHomePage = do
    let dbPath = "transactions.db"


    let bankDir = "bank_files"
    let ccDir = "credit_card_files"

    bankFiles <- Prelude.map (bankDir </>) <$> listDirectory bankDir
    ccFiles <- Prelude.map (ccDir </>) <$> listDirectory ccDir

    let ccCategories = ["Groceries", "Travel","Gas", "Misc", "Subscriptions", "Food"]
    let bankCategories = ["Investments", "Income", "Transfers", "Credit Card Payments", "Insurance"]

    initializeDatabase dbPath

    categorizedBankTransactions <- Prelude.concat <$> mapM (\file -> processPdfFile dbPath file BankSource bankCategories) bankFiles
    categorizedCCTransactions <- Prelude.concat <$> mapM (\file -> processPdfFile dbPath file CreditCardSource ccCategories) ccFiles

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

    let strictText = generateHtmlBlaze bankSummaryRows ccSummaryRows bankRows creditCardRows  bankMonthRows ccMonthRows  sankeyData
    return  strictText