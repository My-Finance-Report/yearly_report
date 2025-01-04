{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HomePage
  ( renderHomePage,
  )
where

import Categorizer
import Control.Monad (forM, forM_)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import Database
import GHC.RTS.Flags (TraceFlags)
import HtmlGenerators.HtmlGenerators
import Parsers
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types

truncateToTwoDecimals :: Double -> Double
truncateToTwoDecimals x = fromIntegral (truncate (x * 100)) / 100

prettyFormat :: Day -> Text
prettyFormat = T.pack . formatTime defaultTimeLocale "%B %Y"

formatSankeyRow :: (Text, Text, Double) -> Text
formatSankeyRow (from, to, weight) =
  "['" <> from <> "', '" <> to <> "', " <> T.pack (show weight) <> "],\n"

generateRow :: CategorizedTransaction -> Html
generateRow categorizedTransaction =
  let txnCategory = category categorizedTransaction
      innerTransaction = transaction categorizedTransaction
      dateStr = formatTime defaultTimeLocale "%B %Y" (transactionDate innerTransaction)
      desc = description innerTransaction
      txnKind = T.pack (show (kind innerTransaction))
      amt = truncateToTwoDecimals (amount innerTransaction)
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
      ( \cat txs accHtml ->
          let totalAmount =
                truncateToTwoDecimals $
                  sum
                    ( Prelude.map
                        ( \txn ->
                            case kind (transaction txn) of
                              Deposit -> amount (transaction txn)
                              Withdrawal -> -amount (transaction txn)
                        )
                        txs
                    )
              sectionId = T.replace " " "-" cat
           in do
                generateAggregateRow cat totalAmount sectionId
                generateDetailRows cat txs sectionId
                accHtml
      )
      (return ()) -- starting "Html" is 'return ()'
      aggregatedTransactions

data SankeyConfig = SankeyConfig
  { sourceKey :: TransactionSource, -- The key for the source node
    targetKey :: TransactionSource, -- The key for the target node
    excludeKeys :: [Text], -- Categories to exclude from processing
    mapKeyFunction :: Text -> Text -- Function to map node names (if needed)
  }

generateSankeyData ::
  Map.Map TransactionSource AggregatedTransactions ->
  SankeyConfig ->
  [(Text, Text, Double)]
generateSankeyData aggregatedTransactions config =
  let SankeyConfig {sourceKey, targetKey, excludeKeys, mapKeyFunction} = config

      -- Fetch and process flows from the sourceKey
      flowsFromSource = case Map.lookup sourceKey aggregatedTransactions of
        Just sourceAggregated ->
          let filteredCategories =
                Map.filterWithKey
                  (\k _ -> notElem k excludeKeys)
                  sourceAggregated
              categoryTotals =
                Map.map (sum . Prelude.map (signedAmount . transaction)) filteredCategories
           in Prelude.map (\(category, total) -> (mapKeyFunction (sourceName sourceKey), category, abs total)) (Map.toList categoryTotals)
        Nothing -> []

      -- Fetch and process flows to the targetKey
      flowsToTarget = case Map.lookup targetKey aggregatedTransactions of
        Just targetAggregated ->
          let totalAmount =
                sum $
                  Map.elems targetAggregated >>= \transactions ->
                    Prelude.map (signedAmount . transaction) transactions
           in [(mapKeyFunction (sourceName sourceKey), mapKeyFunction (sourceName targetKey), abs totalAmount) | totalAmount /= 0]
        Nothing -> []

      -- Combine all flows
      allFlows = flowsFromSource ++ flowsToTarget
   in allFlows

signedAmount :: Transaction -> Double
signedAmount txn = case kind txn of
  Deposit -> amount txn
  Withdrawal -> negate (amount txn)

aggregateByMonth :: [CategorizedTransaction] -> AggregatedTransactions
aggregateByMonth transactions =
  let toYearMonthText txn =
        T.pack $ formatTime defaultTimeLocale "%B %Y" (transactionDate $ transaction txn)
   in Map.fromListWith (++) [(toYearMonthText txn, [txn]) | txn <- transactions]

aggregateByCategory :: [CategorizedTransaction] -> Map.Map TransactionSource AggregatedTransactions
aggregateByCategory = Prelude.foldr insertTransaction Map.empty
  where
    insertTransaction :: CategorizedTransaction -> Map.Map TransactionSource AggregatedTransactions -> Map.Map TransactionSource AggregatedTransactions
    insertTransaction categorizedTransaction acc =
      let source = transactionSource categorizedTransaction
          cat = category categorizedTransaction
          -- Find the existing AggregatedTransactions for the source, or initialize an empty Map
          updatedAggregated = Map.insertWith (++) cat [categorizedTransaction] (Map.findWithDefault Map.empty source acc)
       in Map.insert source updatedAggregated acc

generateHtmlBlaze ::
  [(T.Text, T.Text, Double)] ->
  TL.Text
generateHtmlBlaze sankeyData =
  renderHtml $ do
    H.docTypeHtml $ do
      H.head $ do
        H.title "Expense Summary"

        H.link
          ! A.rel "stylesheet"
          ! A.type_ "text/css"
          ! A.href "/style.css"

        H.script ! A.type_ "text/javascript" ! A.src "https://www.gstatic.com/charts/loader.js" $ mempty
        -- Inline script for Sankey
        H.script ! A.type_ "text/javascript" $
          H.toHtml $
            T.concat
              [ "google.charts.load('current', {packages:['sankey']});\n",
                "google.charts.setOnLoadCallback(drawChart);\n",
                "function drawChart() {\n",
                "  const data = new google.visualization.DataTable();\n",
                "  data.addColumn('string', 'From');\n",
                "  data.addColumn('string', 'To');\n",
                "  data.addColumn('number', 'Weight');\n",
                "  data.addRows([\n",
                T.concat (Prelude.map formatSankeyRow sankeyData),
                "  ]);\n",
                "  const options = { width: 800, height: 600 };\n",
                "  const chart = new google.visualization.Sankey(document.getElementById('sankey_chart'));\n",
                "  chart.draw(data, options);\n",
                "}\n"
              ]
        -- Inline script for toggling details
        H.script $
          H.toHtml $
            T.concat
              [ "function toggleDetails(id) {\n",
                "  var element = document.getElementById(id);\n",
                "  if (element.classList.contains('hidden')) {\n",
                "    element.classList.remove('hidden');\n",
                "  } else {\n",
                "    element.classList.add('hidden');\n",
                "  }\n",
                "}\n"
              ]
      H.body $ do
        H.h1 "Bank Summary"

        H.h1 "Flow Diagram"
        H.div ! A.id "sankey_chart" $ mempty

renderHomePage :: IO TL.LazyText
renderHomePage = do
  let dbPath = "transactions.db"

  -- Fetch all transaction sources
  transactionSources <- getAllTransactionSources dbPath

  categorizedTransactions <- getAllTransactions dbPath

  -- Aggregate transactions
  let aggregatedTransactions = aggregateByCategory categorizedTransactions

  let bankConfig =
        SankeyConfig
          { sourceKey = TransactionSource {sourceName = "Bank"},
            targetKey = TransactionSource {sourceName = "CreditCard"},
            excludeKeys = ["Income", "Credit Card Payments"],
            mapKeyFunction = Prelude.id -- No transformation of node names
          }

  -- Generate Sankey data using the modular function
  let sankeyData = generateSankeyData aggregatedTransactions bankConfig

  -- Generate and return HTML
  let strictText = generateHtmlBlaze sankeyData
  return strictText
