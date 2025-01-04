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
import Data.Ord (Down (Down), comparing)
import Data.Text as T hiding (concatMap, elem)
import qualified Data.Text.Lazy as TL
import Data.Time
import Database
import HtmlGenerators.HtmlGenerators
import Parsers
import Sankey
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

-- Generate tab navigation
generateTabs :: [Text] -> Html
generateTabs tabs =
  H.ul ! A.class_ "tabs" $ do
    forM_ (Prelude.zip [0 ..] tabs) $ \(idx, tab) -> do
      H.li ! A.class_ "tab" ! A.onclick (H.toValue $ "showTab(" <> show idx <> ")") $ toHtml tab

-- Generate content for each tab
generateTabContent :: [(Text, Html)] -> Html
generateTabContent contents =
  H.div ! A.class_ "tab-content-container" $ do
    forM_ (Prelude.zip [0 ..] contents) $ \(idx, (label, content)) -> do
      H.div
        ! A.class_ "tab-content"
        ! A.id (toValue $ "tab-content-" <> show idx)
        ! A.style (if idx == 0 then "display: block;" else "display: none;")
        $ do
          H.h2 (toHtml label)
          content

-- Generate aggregated data by source
generateAggregatedDataBySource :: Map.Map TransactionSource AggregatedTransactions -> Html
generateAggregatedDataBySource aggregatedTransactions =
  generateTabContent $
    Map.foldrWithKey
      (\source aggregated acc -> acc ++ [(sourceName source, generateAggregateRows aggregated)])
      []
      aggregatedTransactions

aggregateByMonth :: [CategorizedTransaction] -> AggregatedTransactions
aggregateByMonth transactions =
  let toYearMonthText txn =
        T.pack $ formatTime defaultTimeLocale "%B %Y" (transactionDate $ transaction txn)
   in Map.fromListWith (++) [(toYearMonthText txn, [txn]) | txn <- transactions]

-- Generate aggregated data by category or month
generateAggregatedDataByGrouping :: [CategorizedTransaction] -> Html
generateAggregatedDataByGrouping transactions =
  let byMonth = aggregateByMonth transactions
      byCategory = Map.unionsWith (++) (Map.elems $ aggregateByCategory transactions)
   in generateTabContent
        [ ("By Month", generateAggregateRows byMonth),
          ("By Category", generateAggregateRows byCategory)
        ]

toggleTabsScript :: Text
toggleTabsScript =
  T.concat
    [ "function showTab(index) {",
      "  var tabs = document.getElementsByClassName('tab-content');",
      "  for (var i = 0; i < tabs.length; i++) {",
      "    tabs[i].style.display = (i === index) ? 'block' : 'none';",
      "  }",
      "}"
    ]

generateHtmlBlaze ::
  [(T.Text, T.Text, Double)] ->
  Maybe Text ->
  Map.Map TransactionSource AggregatedTransactions ->
  Html ->
  TL.Text
generateHtmlBlaze sankeyData banner aggregatedTransactions tabs =
  renderHtml $ do
    H.docTypeHtml $ do
      H.head $ do
        H.title "Expense Summary"

        H.link
          ! A.rel "stylesheet"
          ! A.type_ "text/css"
          ! A.href "/style.css"

        H.script ! A.type_ "text/javascript" ! A.src "/tabs.js" $ mempty
        H.script ! A.type_ "text/javascript" ! A.src "https://www.gstatic.com/charts/loader.js" $ mempty
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
        case banner of
          Just bannerText ->
            H.div ! A.class_ "banner" $ toHtml bannerText
          Nothing -> return ()

        H.div ! A.class_ "upload-section" $ do
          H.h2 "Upload a PDF"
          H.form
            ! A.method "post"
            ! A.action "/upload"
            ! A.enctype "multipart/form-data"
            $ do
              H.label "Choose PDF to upload:"
              H.br
              H.input ! A.type_ "file" ! A.name "pdfFile"
              H.br
              H.input ! A.type_ "submit" ! A.value "Upload"

        H.div ! A.id "sankey_chart" $ mempty
        tabs

generateTransactionSourceTabs :: [TransactionSource] -> Map.Map TransactionSource AggregatedTransactions -> Html
generateTransactionSourceTabs transactionSources aggregatedTransactions = do
  H.div ! A.class_ "tabs-container" $ do
    H.ul ! A.class_ "tabs" $ do
      forM_ (Prelude.zip [0 ..] transactionSources) $ \(idx, source) -> do
        H.li
          ! A.class_ "tab"
          ! A.onclick (H.toValue $ "showTab(" <> show idx <> ")")
          $ toHtml (sourceName source)

  H.div ! A.class_ "tab-content-container" $ do
    forM_ (Prelude.zip [0 ..] transactionSources) $ \(idx, source) -> do
      H.div
        ! A.class_ "tab-content"
        ! A.id (toValue $ "tab-content-" <> show idx)
        ! A.style (if idx == 0 then "display: block;" else "display: none;")
        $ do
          H.h2 (toHtml (sourceName source))
          case Map.lookup source aggregatedTransactions of
            Just aggregated -> generateAggregateRows aggregated
            Nothing -> H.p "No data available for this source."

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

-- General aggregator function
aggregateTransactions ::
  (Ord k) =>
  (CategorizedTransaction -> k) -> -- Key generation function
  [CategorizedTransaction] ->
  Map.Map k [CategorizedTransaction]
aggregateTransactions keyFn transactions =
  Map.fromListWith (++) [(keyFn txn, [txn]) | txn <- transactions]

-- Generate aggregated rows
generateAggregatedRows :: (Ord k, Show k) => Map.Map k [CategorizedTransaction] -> Html
generateAggregatedRows aggregated =
  H.table $ do
    H.tr $ do
      H.th "Key"
      H.th "Total Amount"
    Map.foldrWithKey
      ( \key txns accHtml ->
          let totalAmount =
                truncateToTwoDecimals $
                  sum
                    [ case kind (transaction txn) of
                        Deposit -> amount (transaction txn)
                        Withdrawal -> -amount (transaction txn)
                      | txn <- txns
                    ]
           in do
                H.tr $ do
                  H.td (toHtml (show key))
                  H.td (toHtml (show totalAmount))
                accHtml
      )
      (return ())
      aggregated

generateSubTabContent :: Map.Map TransactionSource [CategorizedTransaction] -> Html
generateSubTabContent aggregatedBySource =
  H.div ! A.class_ "subtab-content-container" $ do
    H.div ! A.class_ "subtab-content" $ do
      H.h2 "By Category"
      generateAggregatedRows (aggregateTransactions category $ concatMap snd $ Map.toList aggregatedBySource)

    H.div ! A.class_ "subtab-content hidden" $ do
      H.h2 "By Month"
      generateAggregatedRows (aggregateTransactions (T.pack . formatTime defaultTimeLocale "%B %Y" . transactionDate . transaction) $ concatMap snd $ Map.toList aggregatedBySource)

-- Generate tabs with subtabs
generateTabsWithSubTabs :: [TransactionSource] -> Map.Map TransactionSource [CategorizedTransaction] -> Html
generateTabsWithSubTabs transactionSources aggregatedBySource =
  H.div ! A.class_ "tabs-container" $ do
    H.ul ! A.class_ "tabs" $ do
      forM_ (Prelude.zip [0 ..] transactionSources) $ \(idx, source) -> do
        H.li
          ! A.class_ "tab"
          ! A.onclick (H.toValue $ "showTabWithSubtabs(" <> show idx <> ")")
          $ toHtml (sourceName source)

    H.div ! A.class_ "tab-content-container" $ do
      forM_ (Prelude.zip [0 ..] transactionSources) $ \(idx, source) -> do
        H.div
          ! A.class_ "tab-content"
          ! A.id (toValue $ "tab-content-" <> show idx)
          ! A.style (if idx == 0 then "display: block;" else "display: none;")
          $ do
            H.h2 (toHtml (sourceName source))
            generateSubTabContent $ Map.filterWithKey (\s _ -> s == source) aggregatedBySource

groupBySource :: [CategorizedTransaction] -> Map.Map TransactionSource [CategorizedTransaction]
groupBySource transactions =
  Map.fromListWith (++) [(transactionSource (category txn), [txn]) | txn <- transactions]

renderHomePage :: Maybe Text -> IO TL.LazyText
renderHomePage banner = do
  let dbPath = "transactions.db"

  transactionSources <- getAllTransactionSources dbPath
  categorizedTransactions <- getAllTransactions dbPath

  let groupedBySource = groupBySource categorizedTransactions :: Map.Map TransactionSource [CategorizedTransaction]

  bankSource <- getTransactionSourceText dbPath "Bank"
  creditCardSource <- getTransactionSourceText dbPath "CreditCard"

  let sankeyConfig =
        SankeyConfig
          { inputs = [(bankSource, "Income")],
            linkages =
              (bankSource, "Credit Card Payments", creditCardSource),
            mapKeyFunction = sourceName
          }

  let sankeyData = generateSankeyData aggregatedBySource sankeyConfig

  let tabs = generateTabsWithSubTabs transactionSources groupedBySource

  let strictText = generateHtmlBlaze sankeyData banner aggregatedBySource tabs
  return strictText
