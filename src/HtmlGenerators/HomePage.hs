{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HomePage
  ( renderHomePage,
  )
where

import Categorizer
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (null, sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Text as T hiding (concatMap, elem)
import qualified Data.Text.Lazy as TL
import Data.Time
import Database.Database
import Database.Files
import Database.Models
import Database.Persist
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import Database.Transaction
import Database.TransactionSource
import HtmlGenerators.Components (navigationBar)
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

generateHomapageHtml ::
  Maybe Text ->
  Html ->
  Html
generateHomapageHtml banner tabs =
  H.body $ do
    generateHeader
    case banner of
      Just bannerText ->
        H.div ! A.class_ "banner" $ toHtml bannerText
      Nothing -> return ()

    H.div ! A.class_ "container" $ do
      H.div ! A.class_ "page-header" $ do
        H.div ! A.class_ "upload-section" $ do
          H.form
            ! A.action "/upload"
            ! A.method "post"
            ! A.class_ "upload_form"
            ! A.enctype "multipart/form-data"
            $ do
              H.input
                ! A.type_ "file"
                ! A.name "pdfFile"
                ! A.accept "application/pdf"
                ! A.id "pdfFileInput"
              H.button
                ! A.type_ "submit"
                ! A.class_ "btn upload-btn"
                ! A.id "uploadButton"
                $ "Add Transactions"

      H.div ! A.class_ "charts-grid" $ do
        H.div ! A.class_ "chart-card" $ do
          H.div
            ! A.id "sankeyChart"
            ! A.class_ "chart sankey-chart"
            $ ""

        H.div ! A.class_ "chart-card" $ do
          H.div
            ! A.id "histogram_chart"
            ! A.class_ "chart histogram-chart"
            $ ""

      tabs

    -- Scripts at the end of body
    H.script
      ! A.type_ "text/javascript"
      ! A.src "https://cdn.jsdelivr.net/npm/interactjs/dist/interact.min.js"
      $ mempty
    H.script
      ! A.type_ "text/javascript"
      ! A.src "https://www.gstatic.com/charts/loader.js"
      $ mempty
    H.script
      ! A.type_ "text/javascript"
      ! A.src "/sankey.js"
      $ mempty
    H.script
      ! A.type_ "text/javascript"
      ! A.src "/toggle.js"
      $ mempty
    H.script
      ! A.type_ "text/javascript"
      ! A.src "/tabs.js"
      $ mempty
    H.script
      ! A.type_ "text/javascript"
      ! A.src "/histogram.js"
      $ mempty
    H.script
      ! A.type_ "text/javascript"
      ! A.src "/resizable.js"
      $ mempty

generateProcessedFilesComponent :: [SourceFileMapping] -> Html
generateProcessedFilesComponent processedFiles = do
  H.div ! A.class_ "processed-files-section" $ do
    if Data.List.null processedFiles
      then H.p "No files have been processed yet."
      else H.table $ do
        H.tr $ do
          H.th "Transaction Source"
          H.th "Filenames"
          H.th "Actions"
        forM_ processedFiles $ \mapping -> do
          forM_ (handledFiles mapping) $ \filename -> do
            H.tr $ do
              H.td (toHtml (transactionSourceName $ entityVal (Types.source mapping)))
              H.td (toHtml filename)
              H.td $ do
                H.form
                  ! A.method "post"
                  ! A.action (toValue $ "/delete-processed-file?filename=" <> T.unpack filename)
                  $ do
                    H.input ! A.type_ "hidden" ! A.name "transactionSourceId" ! A.value (toValue $ show $ entityKey (Types.source mapping))
                    H.input ! A.type_ "hidden" ! A.name "filename" ! A.value (toValue filename)
                    H.input ! A.type_ "submit" ! A.value "Delete"

generateHistogramDiv :: Html
generateHistogramDiv =
  H.div ! A.id "histogram_chart" $ mempty

generateHeader :: Html
generateHeader =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Expense Summary"

      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "/style.css"

      H.script ! A.type_ "text/javascript" ! A.src "https://www.gstatic.com/charts/loader.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "/sankey.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "/tabs.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "/histogram.js" $ mempty

generateSubTabContent :: Int -> Map.Map (Entity TransactionSource) [CategorizedTransaction] -> Html
generateSubTabContent index aggregatedBySource =
  H.div ! A.class_ "subtab-content-container" $ do
    -- Generate tabs for subtabs
    H.ul ! A.class_ "tabs" $ do
      forM_ (Prelude.zip [0 ..] subtabMappings) $ \(idx, (name, _)) -> do
        H.li
          ! A.class_ (if idx == 0 then "tab active" else "tab")
          ! A.onclick (H.toValue $ "showSubTab(" <> show index <> "," <> show idx <> ")")
          $ toHtml name

    -- Generate subtab content for each grouping
    forM_ (Prelude.zip [0 ..] subtabMappings) $ \(idx, (subname, groupingFunc)) -> do
      let groupedData = groupingFunc $ concatMap snd $ Map.toList aggregatedBySource
      H.div
        ! A.class_ "subtab-content"
        ! A.style (if idx == 0 then "display: block;" else "display: none;")
        $ generateAggregatedRowsWithExpandableDetails (toHtml subname) groupedData

generateAggregatedRowsWithExpandableDetails :: Html -> Map.Map Text [CategorizedTransaction] -> Html
generateAggregatedRowsWithExpandableDetails header aggregated =
  H.table $ do
    -- Header row
    H.tr $ do
      H.th ! A.class_ "arrow-column" $ ""
      H.th header
      H.th "Total Amount"
    -- Generate rows for each group
    Map.foldrWithKey
      ( \key txns accHtml ->
          let totalAmount =
                truncateToTwoDecimals $
                  sum
                    [ case transactionKind $ entityVal (transaction txn) of
                        Deposit -> transactionAmount $ entityVal (transaction txn)
                        Withdrawal -> transactionAmount $ entityVal (transaction txn)
                      | txn <- txns
                    ]
              sectionId = "details-" <> key -- Unique ID for the expandable section
           in do
                -- Aggregated row
                generateAggregateRow key totalAmount sectionId
                -- Hidden detail rows
                generateDetailRows key txns sectionId
                accHtml
      )
      (return ())
      aggregated

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
  H.tr ! A.class_ "expandable-row" ! A.onclick (H.toValue $ "toggleDetails('" <> sectionId <> "')") $ do
    H.td ! A.class_ "arrow-column" $ H.span "▶"
    H.td (toHtml cat)
    H.td (toHtml (show totalAmt))

generateDetailRows :: T.Text -> [CategorizedTransaction] -> T.Text -> Html
generateDetailRows cat txs sectionId =
  H.tr ! A.id (H.toValue sectionId) ! A.class_ "hidden" $ do
    -- Make a nested table
    H.td ! A.colspan "3" $ do
      H.table $ do
        H.tr $ do
          H.th "Description"
          H.th "Kind"
          H.th "Date"
          H.th "Amount"
          H.th "Action"
        mapM_ (\catTrans -> detailRow (entityKey (transaction catTrans)) (entityVal (transaction catTrans))) txs
  where
    detailRow :: TransactionId -> Transaction -> Html
    detailRow tid t =
      H.tr $ do
        H.td (toHtml (transactionDescription t))
        H.td (toHtml $ show (transactionKind t))
        H.td (toHtml (formatMonthYear $ transactionDateOfTransaction t))
        H.td (toHtml (show (truncateToTwoDecimals (transactionAmount t))))
        H.td $ do
          case transactionUploadedPdfId t of
            Just pdfId ->
              H.a
                ! A.href (H.toValue $ "/transactions/" <> show (fromSqlKey pdfId) <> "#tx-" <> show (fromSqlKey tid))
                ! A.class_ "btn-edit"
                $ "Edit"
            Nothing ->
              H.span "No PDF ID"

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
                    [ case transactionKind $ entityVal (transaction txn) of
                        Deposit -> transactionAmount $ entityVal (transaction txn)
                        Withdrawal -> transactionAmount $ entityVal (transaction txn)
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
  [ ("Category", groupByBlah (categoryName . entityVal . category)),
    ("Month", groupByBlah (formatMonthYear . transactionDateOfTransaction . entityVal . transaction))
  ]

formatMonthYear :: UTCTime -> Text
formatMonthYear utcTime = T.pack (formatTime defaultTimeLocale "%m/%Y" utcTime)

generateTabsWithSubTabs :: [Entity TransactionSource] -> Map.Map (Entity TransactionSource) [CategorizedTransaction] -> [SourceFileMapping] -> Html
generateTabsWithSubTabs transactionSources aggregatedBySource processsedFiles =
  H.div ! A.class_ "tabs-container" $ do
    H.ul ! A.class_ "tabs" $ do
      forM_ (Prelude.zip [0 ..] transactionSources) $ \(idx, source) -> do
        H.li
          ! A.class_ (if idx == 0 then "tab active" else "tab")
          ! A.onclick (H.toValue $ "showTabWithSubtabs(" <> show idx <> ")")
          $ toHtml (transactionSourceName $ entityVal source)
      H.li
        ! A.class_ "tab"
        ! A.onclick (H.toValue $ "showTabWithSubtabs(" <> show (Prelude.length transactionSources) <> ")")
        $ "Processed Files"

    H.div ! A.class_ "tab-content-container" $ do
      forM_ (Prelude.zip [0 ..] transactionSources) $ \(idx, source) -> do
        H.div
          ! A.class_ "tab-content"
          ! A.id (toValue $ "tab-content-" <> show idx)
          ! A.style (if idx == 0 then "display: block;" else "display: none;")
          $ do
            generateSubTabContent idx $ Map.filterWithKey (\s _ -> s == source) aggregatedBySource
      H.div
        ! A.class_ "tab-content"
        ! A.id (toValue $ "tab-content-" <> show (Prelude.length transactionSources))
        ! A.style "display: none;"
        $ generateProcessedFilesComponent processsedFiles

renderHomePage :: Entity User -> Maybe Text -> IO Html
renderHomePage user banner = do
  transactionSources <- getAllTransactionSources user
  categorizedTransactions <- getAllTransactions user
  groupedBySource <- groupTransactionsBySource user categorizedTransactions
  files <- getSourceFileMappings user

  let updatedBanner =
        if Data.List.null categorizedTransactions
          then Just "You need to add transactions to get started."
          else banner

  let tabs = generateTabsWithSubTabs transactionSources groupedBySource files
  let strictText = generateHomapageHtml updatedBanner tabs
  return strictText
