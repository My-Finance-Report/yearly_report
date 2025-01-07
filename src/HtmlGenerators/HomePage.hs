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
import Database.Persist
import Database.Persist.Postgresql (toSqlKey)
import HtmlGenerators.HtmlGenerators
import Models
import NewDatabase
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

generateHomapageHtml ::
  Maybe [(T.Text, T.Text, Double)] ->
  Maybe Text ->
  Html ->
  Map.Map (Entity TransactionSource) [Text] ->
  TL.Text
generateHomapageHtml sankeyData banner tabs files =
  renderHtml $ do
    generateHeader sankeyData
    case banner of
      Just bannerText ->
        H.div ! A.class_ "banner" $ toHtml bannerText
      Nothing -> return ()

    H.body $ do
      generateProcessedFilesComponent files
      generateUpload
      generateSankeyDiv
      tabs

generateProcessedFilesComponent :: Map.Map (Entity TransactionSource) [Text] -> Html
generateProcessedFilesComponent processedFiles = do
  H.div ! A.class_ "processed-files-section" $ do
    H.h2 "Processed Files"
    if Map.null processedFiles
      then H.p "No files have been processed yet."
      else H.table $ do
        H.tr $ do
          H.th "Transaction Source"
          H.th "Filenames"
          H.th "Actions"
        forM_ (Map.toList processedFiles) $ \(transactionSource, filenames) -> do
          forM_ filenames $ \filename -> do
            H.tr $ do
              H.td (toHtml (transactionSourceName $ entityVal transactionSource))
              H.td (toHtml filename)
              H.td $ do
                H.form
                  ! A.method "post"
                  ! A.action (toValue $ "/delete-processed-file?filename=" <> T.unpack filename)
                  $ do
                    H.input ! A.type_ "hidden" ! A.name "transactionSourceId" ! A.value (toValue $ show $ entityKey transactionSource)
                    H.input ! A.type_ "hidden" ! A.name "filename" ! A.value (toValue filename)
                    H.input ! A.type_ "submit" ! A.value "Delete"

generateHeader :: Maybe [(T.Text, T.Text, Double)] -> Html
generateHeader sankeyData =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Expense Summary"

      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/style.css"

      H.script ! A.type_ "text/javascript" ! A.src "/tabs.js" $ mempty
      generateSankeyScript sankeyData

generateSankeyScript :: Maybe [(T.Text, T.Text, Double)] -> Html
generateSankeyScript sankeyData =
  case sankeyData of
    Just rows -> do
      H.script ! A.type_ "text/javascript" ! A.src "https://www.gstatic.com/charts/loader.js" $ mempty
      H.script
        ! A.type_ "text/javascript"
        $ H.toHtml
        $ T.concat
          [ "google.charts.load('current', {packages:['sankey']});\n",
            "google.charts.setOnLoadCallback(drawChart);\n",
            "function drawChart() {\n",
            "  const data = new google.visualization.DataTable();\n",
            "  data.addColumn('string', 'From');\n",
            "  data.addColumn('string', 'To');\n",
            "  data.addColumn('number', 'Weight');\n",
            "  data.addRows([\n",
            T.concat (Prelude.map formatSankeyRow rows),
            "  ]);\n",
            "  const options = { width: 800, height: 600 };\n",
            "  const chart = new google.visualization.Sankey(document.getElementById('sankey_chart'));\n",
            "  chart.draw(data, options);\n",
            "}\n"
          ]
    Nothing -> ""

generateSankeyDiv :: Html
generateSankeyDiv =
  H.div ! A.id "sankey_chart" $ mempty

generateUpload :: Html
generateUpload =
  H.div ! A.class_ "upload-section" $ do
    H.h2 "Upload a PDF File"
    H.form
      ! A.method "post"
      ! A.action "/upload"
      ! A.enctype "multipart/form-data"
      $ do
        H.div ! A.class_ "form-group file-upload-wrapper" $ do
          H.label ! A.for "pdfFile" ! A.class_ "custom-file-label" $ "Choose File"
          H.input
            ! A.type_ "file"
            ! A.name "pdfFile"
            ! A.id "pdfFile"
            ! A.class_ "custom-file-input"
        H.div ! A.class_ "form-group" $ do
          H.button
            ! A.type_ "submit"
            ! A.class_ "btn upload-btn"
            $ "Upload"

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
        H.td (toHtml (transactionDescription t))
        H.td (toHtml (transactionDateOfTransaction t))
        H.td (toHtml (show (truncateToTwoDecimals (transactionAmount t))))

generateAggregatedRows :: Html -> Map.Map Text [CategorizedTransaction] -> Html
generateAggregatedRows header aggregated =
  H.table $ do
    H.tr $ do
      H.th header
      H.th "Total Amount"
    Map.foldrWithKey
      ( \key txns accHtml ->
          let totalAmount =
                truncateToTwoDecimals $
                  sum
                    [ case transactionKind (transaction txn) of
                        Deposit -> transactionAmount (transaction txn)
                        Withdrawal -> -transactionAmount (transaction txn)
                      | txn <- txns
                    ]
           in do
                H.tr $ do
                  H.td (toHtml key)
                  H.td (toHtml (show totalAmount))
                accHtml
      )
      (return ())
      aggregated

type GroupingFunction = [CategorizedTransaction] -> Map.Map Text [CategorizedTransaction]

subtabMappings :: [(Text, GroupingFunction)]
subtabMappings =
  [ ("Category", groupByBlah (categoryName . category)),
    ("Month", groupByBlah (transactionDateOfTransaction . transaction))
  ]

generateSubTabContent :: Int -> Map.Map (Entity TransactionSource) [CategorizedTransaction] -> Html
generateSubTabContent index aggregatedBySource =
  H.div ! A.class_ "subtab-content-container" $ do
    H.ul ! A.class_ "tabs" $ do
      forM_ (Prelude.zip [0 ..] subtabMappings) $ \(idx, (name, _)) -> do
        H.li
          ! A.class_ (if idx == 0 then "tab active" else "tab")
          ! A.onclick (H.toValue $ "showSubTab(" <> show index <> "," <> show idx <> ")")
          $ toHtml name

    forM_ (Prelude.zip [0 ..] subtabMappings) $ \(idx, (subname, groupingFunc)) -> do
      H.div
        ! A.class_ "subtab-content"
        ! A.style (if idx == 0 then "display: block;" else "display: none;")
        $ do
          generateAggregatedRows (toHtml subname) (groupingFunc $ concatMap snd $ Map.toList aggregatedBySource)

generateTabsWithSubTabs :: [Entity TransactionSource] -> Map.Map (Entity TransactionSource) [CategorizedTransaction] -> Html
generateTabsWithSubTabs transactionSources aggregatedBySource =
  H.div ! A.class_ "tabs-container" $ do
    H.ul ! A.class_ "tabs" $ do
      forM_ (Prelude.zip [0 ..] transactionSources) $ \(idx, source) -> do
        H.li
          ! A.class_ (if idx == 0 then "tab active" else "tab")
          ! A.onclick (H.toValue $ "showTabWithSubtabs(" <> show idx <> ")")
          $ toHtml (transactionSourceName $ entityVal source)

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

  transactionSources <- getAllTransactionSources
  categorizedTransactions <- getAllTransactions

  groupedBySource <- groupTransactionsBySource categorizedTransactions

  sankeyConfig <- getFirstSankeyConfig
  let sankeyData = case sankeyConfig of
        Just config -> Just (generateSankeyData groupedBySource config)
        Nothing -> Nothing

  let tabs = generateTabsWithSubTabs transactionSources groupedBySource

  -- TODO
  -- files <- getTransactionSourceFiles dbPath
  let files = Map.empty

  let strictText = generateHomapageHtml sankeyData banner tabs files
  return strictText
