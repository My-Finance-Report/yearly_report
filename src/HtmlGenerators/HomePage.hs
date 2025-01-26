{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HomePage
  ( renderHomePage,
    makeSimpleBanner,
    makeDemoBanner,
  )
where

import Categorizer
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List
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
import HtmlGenerators.HtmlGenerators
import Parsers
import Sankey
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)
import Types

formatCurrency :: Double -> Text
formatCurrency x
  | x < 0 = "-$" `append` pack (printf "%.2f" (abs x))
  | otherwise = "$" `append` pack (printf "%.2f" x)

formatSankeyRow :: (Text, Text, Double) -> Text
formatSankeyRow (from, to, weight) =
  "['" <> from <> "', '" <> to <> "', " <> T.pack (show weight) <> "],\n"

makeSimpleBanner :: Text -> Html
makeSimpleBanner banner = H.div ! A.class_ "bg-yellow-500 text-black text-center p-3 rounded-md" $ toHtml banner

makeDemoBanner :: Html
makeDemoBanner =
  H.div ! A.class_ "bg-yellow-500 text-black text-center p-3 rounded-md" $ do
    H.span "You are in demo mode. "
    H.a ! A.href "/login" ! A.class_ "underline" $ "Sign up now"

makeCharts :: Html
makeCharts =
  H.div ! A.class_ "flex flex-col md:flex-row gap-6  rounded-md p-4" $ do
    H.div ! A.class_ "flex-1 border border-primary rounded-md p-4 shadow-md min-h-[500px]" $ do
      H.div
        ! A.id "sankeyChart"
        ! A.class_ "sankey-chart w-full h-full"
        $ ""

    H.div ! A.class_ "flex-1 border border-primary rounded-md p-4 shadow-md min-h-[500px]" $ do
      H.div
        ! A.id "histogram_chart"
        ! A.class_ "histogram-chart w-full h-full"
        $ ""

generateHomapageHtml ::
  Maybe Html ->
  Html ->
  Html
generateHomapageHtml banner tabs =
  H.body $ do
    generateHeader
    case banner of
      Just bannerHtml -> bannerHtml
      Nothing -> return ()

    H.div ! A.class_ "" $ do
      makeCharts
      tabs

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
      ! A.src "/tabs.js"
      $ mempty
    H.script
      ! A.type_ "text/javascript"
      ! A.src "/toggle.js"
      $ mempty
    H.script
      ! A.type_ "text/javascript"
      ! A.src "/histogram.js"
      $ mempty

generateProcessedFilesComponent :: [Entity ProcessedFile] -> Html
generateProcessedFilesComponent processedFiles = do
  H.div ! A.class_ "processed-files-section p-6 bg-white rounded-lg shadow-md" $ do
    if Data.List.null processedFiles
      then H.p ! A.class_ "text-gray-500 text-center" $ "No files have been processed yet."
      else H.table ! A.class_ "base-table striped-table hover-table border rounded-lg w-full" $ do
        -- Table Header
        H.thead ! A.class_ "table-head" $ do
          H.tr $ do
            H.th ! A.class_ "table-cell px-4 py-3" $ "Filename"
            H.th ! A.class_ "table-cell px-4 py-3" $ "Status"
            H.th ! A.class_ "table-cell px-4 py-3 text-center" $ "Actions"

        -- Table Rows
        H.tbody $ forM_ processedFiles $ \(Entity processedFileId processedFile) -> do
          H.tr ! A.class_ "table-row hover:bg-gray-50 transition" $ do
            -- Filename Column
            H.td ! A.class_ "table-cell px-4 py-3" $
              toHtml (processedFileFilename processedFile)

            -- Status Column
            H.td ! A.class_ "table-cell px-4 py-3 font-medium text-gray-700" $
              toHtml $
                show (processedFileStatus processedFile)

            -- Actions Column
            H.td ! A.class_ "table-cell-center px-4 py-3 flex justify-center items-center gap-4" $ do
              -- Reprocess Button
              H.form
                ! A.method "post"
                ! A.action (H.toValue $ "/reprocess-file/" <> show (fromSqlKey processedFileId))
                $ do
                  H.input
                    ! A.type_ "hidden"
                    ! A.name "fId"
                    ! A.value (H.toValue $ show (fromSqlKey processedFileId))
                  H.button
                    ! A.type_ "submit"
                    ! A.class_ "secondary-button"
                    $ "Reprocess"

              -- Delete Button
              H.form
                ! A.method "post"
                ! A.action (H.toValue $ "/delete-file/" <> show (fromSqlKey processedFileId))
                $ do
                  H.input
                    ! A.type_ "hidden"
                    ! A.name "fId"
                    ! A.value (H.toValue $ show (fromSqlKey processedFileId))
                  H.button
                    ! A.type_ "submit"
                    ! A.class_ "secondary-danger-button"
                    $ "Delete File and Transactions"

generateHistogramDiv :: Html
generateHistogramDiv =
  H.div ! A.id "histogram_chart" $ mempty

generateHeader :: Html
generateHeader =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Expense Summary"

      H.script ! A.type_ "text/javascript" ! A.src "https://www.gstatic.com/charts/loader.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "/sankey.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "/tabs.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "/histogram.js" $ mempty

generateSubTabContent :: Int -> Map.Map (Entity TransactionSource) [CategorizedTransaction] -> Html
generateSubTabContent index aggregatedBySource =
  H.div ! A.class_ "subtab-content-container flex flex-col w-full" $ do
    H.div ! A.class_ "flex flex-row items-center justify-center mt-4" $ do
      H.div ! A.class_ "flex flex-row gap-2 text-primary border-primary rounded-md border-[1px] p-4 bg-white shadow-sm" $ do
        forM_ (Prelude.zip [0 ..] subtabMappings) $ \(idx, (name, _)) -> do
          H.button
            ! A.type_ "button"
            ! A.class_ "subtab-button secondary-button"
            ! H.dataAttribute "subtab-id" (H.toValue $ "subtab-content-" <> show index <> "-" <> show idx)
            ! A.onclick (H.toValue $ "showSubTab(" <> show index <> "," <> show idx <> ")")
            $ toHtml name

    H.div ! A.class_ "border border-primary p-2 rounded-md mt-4" $ do
      forM_ (Prelude.zip [0 ..] subtabMappings) $ \(idx, (subname, groupingFunc)) -> do
        let groupedData = groupingFunc $ concatMap snd $ Map.toList aggregatedBySource
        H.div
          ! A.class_ "subtab-content"
          ! A.id (H.toValue $ "subtab-content-" <> show index <> "-" <> show idx)
          ! A.style (if idx == 0 then "display: block;" else "display: none;")
          $ generateAggregatedRowsWithExpandableDetails (toHtml subname) groupedData

generateAggregatedRowsWithExpandableDetails :: Html -> Map.Map Text [CategorizedTransaction] -> Html
generateAggregatedRowsWithExpandableDetails header aggregated = do
  let (totalBalance, totalWithdrawals, totalDeposits) = computeTotals aggregated

  H.table ! A.class_ "base-table hover-table striped-table" $ do
    -- Table Header
    generateTableHeader header

    -- Table Rows (Grouped Aggregates + Expandable Details)
    Map.foldrWithKey generateAggregatedSection (return ()) aggregated

    -- Totals Row
    generateTotalsRow totalWithdrawals totalDeposits totalBalance

-- Compute total balance, withdrawals, and deposits
computeTotals :: Map.Map Text [CategorizedTransaction] -> (Text, Text, Text)
computeTotals aggregated =
  let sumTransactions f =
        formatCurrency $
          sum [f (entityVal (transaction txn)) | txns <- Map.elems aggregated, txn <- txns]
   in ( sumTransactions computeBalance,
        sumTransactions computeWithdrawals,
        sumTransactions computeDeposits
      )

computeBalance :: Transaction -> Double
computeBalance txn =
  case transactionKind txn of
    Deposit -> transactionAmount txn
    Withdrawal -> negate $ transactionAmount txn

computeWithdrawals :: Transaction -> Double
computeWithdrawals txn =
  case transactionKind txn of
    Deposit -> 0
    Withdrawal -> transactionAmount txn

computeDeposits :: Transaction -> Double
computeDeposits txn =
  case transactionKind txn of
    Deposit -> transactionAmount txn
    Withdrawal -> 0

-- Generate Table Header
generateTableHeader :: Html -> Html
generateTableHeader header = do
  H.thead ! A.class_ "table-head" $ do
    H.tr ! A.class_ "" $ do
      H.th ! A.class_ "table-cell w-6 text-center p-2 border-primary border-l border-b border-t" $ ""
      H.th ! A.class_ "table-cell p-2 border-t border-b border-primary font-semibold" $ header
      H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Withdrawals"
      H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Deposits"
      H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Balance"

-- Generate Aggregated Section with Expandable Details
generateAggregatedSection :: Text -> [CategorizedTransaction] -> Html -> Html
generateAggregatedSection key txns accHtml = do
  let (balance, withdrawals, deposits) = computeGroupTotals txns
      sectionId = "details-" <> key <> (T.pack . show . fromSqlKey . entityKey . transaction . Data.List.head $ txns)

  generateAggregateRow key balance withdrawals deposits sectionId
  generateDetailRows key txns sectionId
  accHtml

-- Compute totals for a given transaction group
computeGroupTotals :: [CategorizedTransaction] -> (Text, Text, Text)
computeGroupTotals txns =
  ( formatCurrency $ sum $ Prelude.map (computeBalance . entityVal . transaction) txns,
    formatCurrency $ sum $ Prelude.map (computeWithdrawals . entityVal . transaction) txns,
    formatCurrency $ sum $ Prelude.map (computeDeposits . entityVal . transaction) txns
  )

-- Generate Totals Row
generateTotalsRow :: Text -> Text -> Text -> Html
generateTotalsRow totalWithdrawals totalDeposits totalBalance = do
  H.tr ! A.class_ "totals-row font-semibold" $ do
    H.td ! A.colspan "2" ! A.class_ "p-2 border-t border-primary" $ "Totals"
    H.td ! A.class_ "p-2 border-t border-primary" $ toHtml totalWithdrawals
    H.td ! A.class_ "p-2 border-t border-primary" $ toHtml totalDeposits
    H.td ! A.class_ "p-2 border-t border-primary" $ toHtml totalBalance

generateSankeyDiv :: Html
generateSankeyDiv =
  H.div ! A.id "sankey_chart" $ mempty

generateAggregateRow :: Text -> Text -> Text -> Text -> Text -> Html
generateAggregateRow cat balance withdrawls deposits sectionId =
  H.tr ! A.class_ "expandable-row table-row" ! A.onclick (H.toValue $ "toggleDetails('" <> sectionId <> "'); toggleArrow(this)") $ do
    H.td ! A.class_ "table-cell w-6 text-center transition-transform duration-200 ease-in-out" $
      H.span ! A.class_ "inline-block transform" $
        "▶"
    H.td ! A.class_ "table-cell" $ toHtml cat
    H.td ! A.class_ "table-cell" $ toHtml withdrawls
    H.td ! A.class_ "table-cell" $ toHtml deposits
    H.td ! A.class_ "table-cell" $ toHtml balance

generateDetailRows :: T.Text -> [CategorizedTransaction] -> T.Text -> Html
generateDetailRows cat txs sectionId =
  H.tr ! A.id (H.toValue sectionId) ! A.class_ "hidden" $ do
    -- Make a nested table
    H.td ! A.colspan "5" $ do
      H.div ! A.class_ "p-2 border border-gray-300 rounded-md bg-white shadow-sm" $ do
        H.table ! A.class_ "base-table" $ do
          H.thead ! A.class_ "table-head hover" $ do
            H.tr ! A.class_ "" $ do
              H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Transaction"
              H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Kind"
              H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Date"
              H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Amount"
              H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Action"
          H.tbody $ do
            mapM_ (\catTrans -> detailRow (entityKey (transaction catTrans)) (entityVal (transaction catTrans))) txs
  where
    detailRow :: TransactionId -> Transaction -> Html
    detailRow tid t =
      H.tr ! A.class_ "border-b border-gray-200 hover:bg-gray-100 transition-all" $ do
        H.td ! A.class_ "p-2 border border-gray-200" $ toHtml (transactionDescription t)
        H.td ! A.class_ "p-2 border border-gray-200" $ toHtml $ show (transactionKind t)
        H.td ! A.class_ "p-2 border border-gray-200" $ toHtml (formatFullDate $ transactionDateOfTransaction t)
        H.td ! A.class_ "p-2 border border-gray-200" $ toHtml (formatCurrency (transactionAmount t))
        H.td ! A.class_ "p-2 border border-gray-200 text-center" $ do
          case transactionUploadedPdfId t of
            Just pdfId ->
              H.a
                ! A.href (H.toValue $ "/transactions/" <> show (fromSqlKey pdfId) <> "#tx-" <> show (fromSqlKey tid))
                ! A.class_ "secondary-button px-3 py-1 text-sm"
                $ "Edit"
            Nothing ->
              H.span "No PDF ID"

type GroupingFunction = [CategorizedTransaction] -> Map.Map Text [CategorizedTransaction]

subtabMappings :: [(Text, GroupingFunction)]
subtabMappings =
  [ ("Category", groupByBlah (categoryName . entityVal . category)),
    ("Month", groupByBlah (formatMonthYear . transactionDateOfTransaction . entityVal . transaction))
  ]

formatMonthYear :: UTCTime -> Text
formatMonthYear utcTime = T.pack (formatTime defaultTimeLocale "%B %Y" utcTime)

formatFullDate :: UTCTime -> Text
formatFullDate utcTime = T.pack (formatTime defaultTimeLocale "%m/%d/%Y" utcTime)

generateTabsWithSubTabs :: [Entity TransactionSource] -> Map.Map (Entity TransactionSource) [CategorizedTransaction] -> [Entity ProcessedFile] -> Html
generateTabsWithSubTabs transactionSources aggregatedBySource processedFiles =
  H.div ! A.class_ "tabs-container flex flex-col" $ do
    -- Button Group for Tabs
    H.div ! A.class_ "flex flex-row items-center justify-center mt-4" $ do
      H.div ! A.class_ "tabs flex flex-row gap-2 text-primary border-primary rounded-md border-[1px] p-4 bg-white shadow-sm" $ do
        forM_ (Prelude.zip [0 ..] transactionSources) $ \(idx, source) -> do
          H.button
            ! A.type_ "button"
            ! A.class_ "tab-button secondary-button"
            ! H.dataAttribute "tab-id" (H.toValue $ "tab-content-" <> show idx)
            ! A.onclick (H.toValue $ "this.setAttribute('disabled', 'true'); showTabWithSubtabs(" <> show idx <> ");")
            $ toHtml (transactionSourceName $ entityVal source)

        -- Button for Processed Files Tab
        H.button
          ! A.type_ "button"
          ! A.class_ "secondary-button"
          ! H.dataAttribute "tab-id" (H.toValue $ "tab-content-" <> show (Prelude.length transactionSources))
          ! A.onclick (H.toValue $ "showTabWithSubtabs(" <> show (Prelude.length transactionSources) <> ")")
          $ "Processed Files"

    -- Tab Content Sections
    H.div ! A.class_ "border border-primary p-2 rounded-md mt-4" $ do
      forM_ (Prelude.zip [0 ..] transactionSources) $ \(idx, source) -> do
        H.div
          ! A.class_ "tab-content"
          ! A.id (toValue $ "tab-content-" <> show idx)
          ! A.style (if idx == 0 then "display: block;" else "display: none;")
          $ generateSubTabContent idx
          $ Map.filterWithKey (\s _ -> s == source) aggregatedBySource

      -- Processed Files Tab Content
      H.div
        ! A.class_ "tab-content"
        ! A.id (toValue $ "tab-content-" <> show (Prelude.length transactionSources))
        ! A.style "display: none;"
        $ generateProcessedFilesComponent processedFiles

renderHomePage :: Entity User -> Maybe Html -> IO Html
renderHomePage user banner = do
  transactionSources <- getAllTransactionSources user
  categorizedTransactions <- getAllTransactions user
  groupedBySource <- groupTransactionsBySource user categorizedTransactions
  files <- getAllProcessedFiles user

  let updatedBanner = case banner of
        Just existingBanner | not (Prelude.null banner) -> Just existingBanner
        _ ->
          if Data.List.null categorizedTransactions
            then Just $ makeSimpleBanner "You need to add transactions to get started."
            else Nothing

  let tabs = generateTabsWithSubTabs transactionSources groupedBySource files
  let strictText = generateHomapageHtml updatedBanner tabs
  return strictText
