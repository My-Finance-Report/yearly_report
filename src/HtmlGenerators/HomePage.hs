{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HomePage (renderHomePage) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (groupBy, head, null, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text as T hiding (concatMap, elem)
import qualified Data.Text.Lazy as TL
import Database.Models
import Database.Persist
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import Database.Transaction
import Database.TransactionSource
import HtmlGenerators.Components (makeSimpleBanner)
import HtmlGenerators.HomePageHelpers
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types

generateAggregatedRowsWithExpandableDetails ::
  -- | Header for this group
  H.Html ->
  -- | Aggregated data
  Map.Map Text [CategorizedTransaction] ->
  H.Html
generateAggregatedRowsWithExpandableDetails header aggregated = do
  let (totalBalance, totalWithdrawals, totalDeposits) = computeTotals aggregated

  H.table ! A.class_ "base-table hover-table striped-table w-full" $ do
    generateTableHeader header

    -- For each key in the aggregated Map, generate a summary row + hidden detail
    Map.foldrWithKey generateAggregatedSection (return ()) aggregated

    -- Then show the totals row
    generateTotalsRow totalWithdrawals totalDeposits totalBalance

generateTableHeader :: H.Html -> H.Html
generateTableHeader header = do
  H.thead ! A.class_ "table-head" $ do
    H.tr $ do
      H.th ! A.class_ "table-cell w-6 text-center p-2 border-primary border-l border-b border-t" $ ""
      H.th ! A.class_ "table-cell p-2 border-t border-b border-primary font-semibold" $ header
      H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Withdrawals"
      H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Deposits"
      H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Balance"

generateAggregatedSection ::
  -- | group key
  Text ->
  -- | transactions
  [CategorizedTransaction] ->
  -- | accumulator
  H.Html ->
  H.Html
generateAggregatedSection key txns accHtml = do
  let (balance, withdrawals, deposits) = computeGroupTotals txns
      sectionId = "details-" <> key
  generateAggregateRow key balance withdrawals deposits sectionId
  generateDetailRows txns sectionId
  accHtml

generateTotalsRow :: Text -> Text -> Text -> Html
generateTotalsRow totalWithdrawals totalDeposits totalBalance = do
  H.tr ! A.class_ "totals-row font-semibold" $ do
    H.td ! A.colspan "2" ! A.class_ "p-2 border-t border-primary" $ "Totals"
    H.td ! A.class_ "p-2 border-t border-primary" $ toHtml totalWithdrawals
    H.td ! A.class_ "p-2 border-t border-primary" $ toHtml totalDeposits
    H.td ! A.class_ "p-2 border-t border-primary" $ toHtml totalBalance

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

generateDetailRows ::
  [CategorizedTransaction] ->
  Text ->
  H.Html
generateDetailRows txs sectionId =
  H.tr
    ! A.id (toValue sectionId)
    ! A.class_ "hidden"
    $ do
      H.td ! A.colspan "5" $ do
        H.div ! A.class_ "p-2 border border-gray-300 rounded-md bg-white shadow-sm" $ do
          H.table ! A.class_ "base-table w-full" $ do
            H.thead $ do
              H.tr $ do
                H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Transaction"
                H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Kind"
                H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Date"
                H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Amount"
                H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Action"
            H.tbody $ do
              forM_ (Prelude.zip [1 ..] txs) $ \(tid, catTrans) ->
                detailRow tid (entityVal (transaction catTrans))
  where
    detailRow :: Int -> Transaction -> H.Html
    detailRow tid t =
      H.tr ! A.class_ "border-b border-gray-200 hover:bg-gray-100 transition-all" $ do
        H.td ! A.class_ "p-2 border border-gray-200" $ toHtml (transactionDescription t)
        H.td ! A.class_ "p-2 border border-gray-200" $ toHtml $ show $ transactionKind t
        H.td ! A.class_ "p-2 border border-gray-200" $ toHtml (formatFullDate (transactionDateOfTransaction t))
        H.td ! A.class_ "p-2 border border-gray-200" $ toHtml (formatCurrency (transactionAmount t))
        H.td ! A.class_ "p-2 border border-gray-200 text-center" $ do
          case transactionUploadedPdfId t of
            Just pdfId ->
              H.a
                ! A.href (toValue $ "/transactions/" <> show pdfId <> "#tx-" <> show tid)
                ! A.class_ "secondary-button px-3 py-1 text-sm"
                $ "Edit"
            Nothing ->
              H.span "No PDF ID"

generateButtonRow ::
  [Entity TransactionSource] ->
  [(Text, GroupingFunction)] ->
  H.Html
generateButtonRow transactionSources mappings =
  -- You may want to group by sourceKind. This is optional:
  let sortedSources = transactionSources
   in -- group them or sort them as you need:
      -- groupedByKind = groupBy ... (Your existing logic)

      H.div ! A.class_ "flex flex-row items-center justify-between mt-4 bg-white border border-primary rounded-md p-4 shadow-sm w-full" $ do
        -- Left side: Buttons for each TransactionSource
        H.div ! A.class_ "flex flex-row gap-4" $ do
          -- If you want them grouped by kind, do that. Otherwise just map over them:
          forM_ (Prelude.zip [0 ..] sortedSources) $ \(idx, srcEnt) -> do
            let srcName = transactionSourceName (entityVal srcEnt)
            H.button
              ! A.type_ "button"
              ! A.class_ "tab-button secondary-button"
              ! A.onclick (toValue $ "showTabWithSubtabs(" <> show idx <> ");")
              $ toHtml srcName

        -- Right side: Grouping buttons
        H.fieldset ! A.class_ "flex flex-row gap-2 text-primary border-primary rounded-md border-[1px] p-2 bg-white shadow-sm" $ do
          H.legend ! A.class_ "text-lg font-semibold text-primary" $ "Group By"
          forM_ (Prelude.zip [0 ..] mappings) $ \(subIdx, (subName, _)) -> do
            H.button
              ! A.type_ "button"
              ! A.class_ "subtab-button secondary-button"
              ! A.onclick (toValue $ "showSubTab(" <> show subIdx <> ")")
              $ toHtml subName

generateSubTabContent ::
  -- | The source's index
  Int ->
  -- | The subtab index
  Int ->
  -- | The subtab's displayed name
  Text ->
  -- | grouped data
  Map.Map Text [CategorizedTransaction] ->
  H.Html
generateSubTabContent srcIdx subIdx subtabName groupedData = do
  let subtabId = "subtab-content-" <> show srcIdx <> "-" <> show subIdx
  H.div
    ! A.id (toValue subtabId)
    ! A.class_ "subtab-content"
    ! A.style (if subIdx == 0 then "display: block;" else "display: none;")
    $ generateAggregatedRowsWithExpandableDetails (toHtml subtabName) groupedData

generateTabsWithSubTabs ::
  [Entity TransactionSource] ->
  Map.Map (Entity TransactionSource) [CategorizedTransaction] ->
  H.Html
generateTabsWithSubTabs transactionSources aggregatedBySource = do
  H.div ! A.class_ "tabs-container flex flex-col" $ do
    -- The row of buttons:
    generateButtonRow transactionSources subtabMappings

    -- The actual tab contents
    H.div ! A.class_ "border border-primary p-2 rounded-md mt-4" $ do
      -- For each TransactionSource, create one "tab-content" block
      forM_ (Prelude.zip [0 ..] transactionSources) $ \(srcIdx, sourceEnt) -> do
        let tabId = "tab-content-" <> show srcIdx
            -- Filter the big Map down to only the data belonging to `sourceEnt`
            thisSourceData = Map.filterWithKey (\k _ -> k == sourceEnt) aggregatedBySource

        H.div
          ! A.id (toValue tabId)
          ! A.class_ "tab-content"
          ! A.style (if srcIdx == 0 then "display: block;" else "display: none;")
          $ do
            -- For each subtabMapping, we do a grouping
            forM_ (Prelude.zip [0 ..] subtabMappings) $ \(subIdx, (subName, groupingFunc)) -> do
              let allTxs = Prelude.concat (Map.elems thisSourceData)
                  grouped = groupingFunc allTxs
              generateSubTabContent srcIdx subIdx subName grouped

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
      loadScripts
      makeCharts
      tabs

renderHomePage :: Entity User -> Maybe Html -> IO Html
renderHomePage user banner = do
  transactionSources <- getAllTransactionSources user
  categorizedTransactions <- getAllTransactions user
  groupedBySource <- groupTransactionsBySource user categorizedTransactions

  let updatedBanner = case banner of
        Just existingBanner | not (Prelude.null banner) -> Just existingBanner
        _ ->
          if Data.List.null categorizedTransactions
            then Just $ makeSimpleBanner "You need to add transactions to get started."
            else Nothing

  let tabs = generateTabsWithSubTabs transactionSources groupedBySource
  let strictText = generateHomapageHtml updatedBanner tabs
  return strictText
