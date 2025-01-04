{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HomePage
  ( renderHomePage,
  )
where

import Categorizer
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text as T hiding (concatMap, elem)
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
        H.td (toHtml (categoryName txnCategory))
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
  { inputs :: [(TransactionSource, Text)], -- [(Source, Category)]
    linkages :: (TransactionSource, Text, TransactionSource), -- (Source, Category) -> Target Source
    mapKeyFunction :: TransactionSource -> Text -- Mapping for display names
  }

buildSankeyLinks ::
  (TransactionSource, Text, TransactionSource) ->
  Map.Map TransactionSource AggregatedTransactions ->
  [(Text, Text, Double)]
buildSankeyLinks (sourceSource, sourceCategory, targetSource) aggregatedTransactions =
  case Map.lookup targetSource aggregatedTransactions of
    Just targetTransactions ->
      -- Sum the amounts for each key in the map
      let categoryTotals = Map.map (sum . Prelude.map (signedAmount . transaction)) targetTransactions
          -- Generate Sankey data: [sourceCategory, key, value]
          sankeyLinks =
            Prelude.map
              (\(category, total) -> (sourceCategory, category, total))
              (Map.toList categoryTotals)
       in sankeyLinks
    Nothing -> []

-- we can make this easier by summing ahead of time
generateSankeyData ::
  Map.Map TransactionSource AggregatedTransactions ->
  SankeyConfig ->
  [(Text, Text, Double)]
generateSankeyData aggregatedTransactions config =
  let SankeyConfig {inputs, linkages, mapKeyFunction} = config

      -- Generate flows from inputs
      inputFlows = concatMap processInput inputs
        where
          processInput (source, category) =
            case Map.lookup source aggregatedTransactions >>= Map.lookup category of
              Just transactions ->
                let subCategoryFlows =
                      Map.toList (Map.filterWithKey (\k _ -> k /= category) (fromMaybe Map.empty (Map.lookup source aggregatedTransactions)))
                 in Prelude.map (\(cat, txns) -> (category, cat, sum $ Prelude.map (sankeyAmount . transaction) txns)) subCategoryFlows
              Nothing -> []
   in inputFlows ++ (buildSankeyLinks linkages aggregatedTransactions)

sankeyAmount :: Transaction -> Double
sankeyAmount txn = abs $ signedAmount txn

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
      let source = transactionSource (category categorizedTransaction)
          cat = category categorizedTransaction
          -- Find the existing AggregatedTransactions for the source, or initialize an empty Map
          updatedAggregated = Map.insertWith (++) (categoryName cat) [categorizedTransaction] (Map.findWithDefault Map.empty source acc)
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

  bankSource <- getTransactionSourceText dbPath "Bank"
  creditCardSource <- getTransactionSourceText dbPath "CreditCard"

  let sankeyConfig =
        SankeyConfig
          { inputs = [(bankSource, "Income")],
            linkages =
              (bankSource, "Credit Card Payments", creditCardSource),
            mapKeyFunction = sourceName
          }

  -- Generate Sankey data using the modular function
  let sankeyData = generateSankeyData aggregatedTransactions sankeyConfig

  print sankeyData

  -- Generate and return HTML
  let strictText = generateHtmlBlaze sankeyData
  return strictText
