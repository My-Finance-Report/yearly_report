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
import Database
import Database.Persist
import Database.Persist.Postgresql (toSqlKey)
import HtmlGenerators.Components (navigationBar)
import HtmlGenerators.HtmlGenerators
import Models
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
  [SourceFileMapping] ->
  Html
generateHomapageHtml banner tabs files =
      H.body $ do
        generateHeader
        case banner of
          Just bannerText ->
            H.div ! A.class_ "banner" $ toHtml bannerText
          Nothing -> return ()

        H.div $ do
          generateProcessedFilesComponent files
          generateUpload
          generateSankeyDiv
          generateHistogramDiv
          tabs

generateProcessedFilesComponent :: [SourceFileMapping] -> Html
generateProcessedFilesComponent processedFiles = do
  H.div ! A.class_ "processed-files-section" $ do
    H.h2 "Processed Files"
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
    H.ul ! A.class_ "tabs" $ do
      forM_ (Prelude.zip [0 ..] subtabMappings) $ \(idx, (name, _)) -> do
        H.li
          ! A.class_ (if idx == 0 then "tab active" else "tab")
          ! A.onclick (H.toValue $ "showSubTab(" <> show index <> "," <> show idx <> ")")
          $ toHtml name

    forM_ (Prelude.zip [0 ..] subtabMappings) $ \(idx, (subname, groupingFunc)) -> do
      let groupedData = groupingFunc $ concatMap snd $ Map.toList aggregatedBySource
      H.div
        ! A.class_ "subtab-content"
        ! A.style (if idx == 0 then "display: block;" else "display: none;")
        $ do
          generateAggregatedRows (toHtml subname) groupedData

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
        H.td (toHtml (formatMonthYear $ transactionDateOfTransaction t))
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
    ("Month", groupByBlah (formatMonthYear . transactionDateOfTransaction . transaction))
  ]

formatMonthYear :: UTCTime -> Text
formatMonthYear utcTime = T.pack (formatTime defaultTimeLocale "%m/%Y" utcTime)

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

renderHomePage :: Maybe Text -> IO Html
renderHomePage banner = do
  transactionSources <- getAllTransactionSources
  categorizedTransactions <- getAllTransactions
  groupedBySource <- groupTransactionsBySource categorizedTransactions

  let tabs = generateTabsWithSubTabs transactionSources groupedBySource

  files <- getSourceFileMappings

  let strictText = generateHomapageHtml banner tabs files
  return strictText
