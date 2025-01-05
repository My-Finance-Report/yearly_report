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

generateHtmlBlaze ::
  [(T.Text, T.Text, Double)] ->
  Maybe Text ->
  Html ->
  TL.Text
generateHtmlBlaze sankeyData banner tabs =
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

generateSubTabContent :: Int -> Map.Map TransactionSource [CategorizedTransaction] -> Html
generateSubTabContent index aggregatedBySource =
  let blahs = ["Category", "Month"] :: [Text]
   in H.div ! A.class_ "subtab-content-container" $ do
        H.ul ! A.class_ "tabs" $ do
          forM_ (Prelude.zip [0 ..] blahs) $ \(idx, source) -> do
            H.li
              ! A.class_ "tab"
              ! A.onclick (H.toValue $ "showSubTab(" <> show index <> "," <> show idx <> ")")
              $ toHtml source
        H.div ! A.class_ "subtab-content" $ do
          H.h2 "By Category"
          generateAggregatedRows (groupByBlah (categoryName . category) $ concatMap snd $ Map.toList aggregatedBySource)

        H.div ! A.class_ "subtab-content" $ do
          H.h2 "By Month"
          generateAggregatedRows (groupByBlah (T.pack . formatTime defaultTimeLocale "%B %Y" . transactionDate . transaction) $ concatMap snd $ Map.toList aggregatedBySource)

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
            generateSubTabContent idx $ Map.filterWithKey (\s _ -> s == source) aggregatedBySource

renderHomePage :: Maybe Text -> IO TL.LazyText
renderHomePage banner = do
  let dbPath = "transactions.db"

  transactionSources <- getAllTransactionSources dbPath
  categorizedTransactions <- getAllTransactions dbPath

  let groupedBySource = groupByBlah (transactionSource . category) categorizedTransactions

  bankSource <- getTransactionSourceText dbPath "Bank"
  creditCardSource <- getTransactionSourceText dbPath "CreditCard"

  let sankeyConfig =
        SankeyConfig
          { inputs = [(bankSource, "Income")],
            linkages =
              (bankSource, "Credit Card Payments", creditCardSource),
            mapKeyFunction = sourceName
          }

  let sankeyData = generateSankeyData groupedBySource sankeyConfig

  let tabs = generateTabsWithSubTabs transactionSources groupedBySource

  let strictText = generateHtmlBlaze sankeyData banner tabs
  return strictText
