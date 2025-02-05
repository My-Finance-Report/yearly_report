{-# LANGUAGE LambdaCase #-}
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
import HtmlGenerators.Components (makeAddTransactionsBanner, makeSimpleBanner)
import HtmlGenerators.HomePageHelpers
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types

generateButtonRow ::
  [Entity TransactionSource] ->
  [(T.Text, [GroupingFunction])] ->
  Html
generateButtonRow transactionSources mappings =
  let sortedSources = transactionSources
      indexedSources = Prelude.zip [0 ..] sortedSources
      groupedByKind =
        Data.List.groupBy
          ( \(_, entA) (_, entB) ->
              transactionSourceSourceKind (entityVal entA)
                == transactionSourceSourceKind (entityVal entB)
          )
          indexedSources
   in H.div ! A.class_ "flex flex-row flex-wrap gap-6" $ do
        forM_ groupedByKind $ \sameKindGroup ->
          case sameKindGroup of
            [] -> mempty
            ((_, firstEnt) : _) -> do
              let kind = transactionSourceSourceKind (entityVal firstEnt)
              H.fieldset
                ! A.class_
                  "flex flex-row gap-2 text-primary border-primary \
                  \rounded-md border-[1px] p-4 bg-white shadow-sm"
                $ do
                  H.legend ! A.class_ "text-lg font-semibold text-primary" $
                    toHtml (show kind) <> "s"

                  forM_ sameKindGroup $ \(idx, srcEnt) -> do
                    let srcId = entityKey srcEnt
                        srcName = transactionSourceName (entityVal srcEnt)
                    H.button
                      ! A.type_ "button"
                      ! A.class_ "tab-button secondary-button"
                      ! H.dataAttribute "tab-index" (toValue $ Prelude.show idx)
                      ! H.dataAttribute "source-id" (toValue $ show (fromSqlKey srcId))
                      ! A.onclick (toValue $ "showTabWithSubtabs(" <> Prelude.show idx <> ")")
                      $ toHtml srcName
        -- A separate fieldset for "Group By" buttons
        H.fieldset
          ! A.class_
            "flex flex-row gap-2 text-primary border-primary \
            \rounded-md border-[1px] p-4 bg-white shadow-sm"
          $ do
            H.legend ! A.class_ "text-lg font-semibold text-primary" $ "Group By"

            forM_ (Prelude.zip [0 ..] mappings) $ \(subIdx, (subName, _)) -> do
              H.button
                ! A.type_ "button"
                ! A.class_ "subtab-button secondary-button"
                ! H.dataAttribute "subtab-index" (toValue $ Prelude.show subIdx)
                ! H.dataAttribute "group-id" (toValue $ show subIdx)
                ! A.onclick (toValue $ "showSubTab(" <> Prelude.show subIdx <> ")")
                $ toHtml subName

generateTableHeader :: H.Html -> H.Html
generateTableHeader header = do
  H.thead ! A.class_ "table-head" $ do
    H.tr $ do
      -- Empty spacer column
      H.th ! A.class_ "table-cell w-6 text-center p-2 border-primary border-l border-b border-t" $ ""

      -- Dynamic Column Headers
      H.th ! A.class_ "table-cell p-2 border-t border-b border-primary font-semibold" $ header

      -- Desktop View (Withdrawals & Deposits)
      H.th ! A.class_ "table-cell p-2 border border-primary font-semibold hidden md:table-cell" $ "Withdrawals"
      H.th ! A.class_ "table-cell p-2 border border-primary font-semibold hidden md:table-cell" $ "Deposits"

      -- Mobile View (Merged Amount Column)
      H.th ! A.class_ "table-cell p-2 border border-primary font-semibold md:hidden" ! A.colspan "2" $ "Amount"

      -- Balance Column (Always Visible)
      H.th ! A.class_ "table-cell p-2 border border-primary font-semibold" $ "Balance"

generateAggregatedRowsWithExpandableDetails ::
  -- | Header for this group
  H.Html ->
  -- | Aggregated data
  Map.Map Text [CategorizedTransaction] ->
  Int ->
  Text ->
  H.Html
generateAggregatedRowsWithExpandableDetails header aggregated srcIdx subName = do
  let (totalBalance, totalWithdrawals, totalDeposits) = computeTotals aggregated

  H.table ! A.class_ "base-table hover-table striped-table w-full" $ do
    generateTableHeader header

    forM_ (Prelude.zip [0 ..] (Map.toList aggregated)) $ \(localIdx, (key, txns)) -> do
      let (balance, withdrawals, deposits) = computeGroupTotals txns
          -- Create a *globally* unique ID for this row
          -- e.g. "details-0-1-Groceries-0"
          sectionId =
            T.concat
              [ "details-",
                T.pack (show srcIdx), -- e.g. "0"
                "-",
                subName,
                "-",
                key, -- e.g. "Groceries"
                "-",
                T.pack (show localIdx) -- e.g. "0"
              ]

      generateAggregateRow key balance withdrawals deposits sectionId
      generateDetailRows txns sectionId

    -- Finally, show the totals row
    generateTotalsRow totalWithdrawals totalDeposits totalBalance

generateAggregatedSection ::
  Text ->
  [CategorizedTransaction] ->
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
  H.tr ! A.class_ "totals-row font-semibold text-sm md:text-lg" $ do
    -- Totals label (spanning the first two columns)
    H.td ! A.colspan "2" ! A.class_ "p-2 border-t border-primary" $ "Totals"

    -- Withdrawals & Deposits (Desktop Only)
    H.td ! A.class_ "p-2 border-t border-primary hidden md:table-cell" $ toHtml totalWithdrawals
    H.td ! A.class_ "p-2 border-t border-primary hidden md:table-cell" $ toHtml totalDeposits

    -- Merged Amount Column (Mobile Only)
    H.td ! A.class_ "p-2 border-t border-primary md:hidden" ! A.colspan "2" $
      toHtml (totalWithdrawals <> " / " <> totalDeposits)

    -- Balance Column (Always Visible)
    H.td ! A.class_ "p-2 border-t border-primary" $ toHtml totalBalance

generateAggregateRow :: Text -> Text -> Text -> Text -> Text -> Html
generateAggregateRow cat balance withdrawls deposits sectionId =
  H.tr ! A.class_ "expandable-row table-row" ! A.onclick (H.toValue $ "toggleDetails('" <> sectionId <> "'); toggleArrow(this)") $ do
    -- Expand arrow column
    H.td ! A.class_ "table-cell w-6 text-center transition-transform duration-200 ease-in-out" $
      H.span ! A.class_ "inline-block transform" $
        "▶"

    -- Category Column
    H.td ! A.class_ "table-cell" $ toHtml cat

    -- Withdrawals & Deposits (Desktop Only)
    H.td ! A.class_ "table-cell hidden md:table-cell" $ toHtml withdrawls
    H.td ! A.class_ "table-cell hidden md:table-cell" $ toHtml deposits

    -- Merged Amount Column (Mobile Only)
    H.td ! A.class_ "table-cell md:hidden" ! A.colspan "2" $ toHtml (withdrawls <> " / " <> deposits)

    -- Balance Column (Always Visible)
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

generateNestedTable ::
  Int ->
  Int ->
  Text ->
  GroupedTransactions ->
  H.Html
generateNestedTable srcIdx subIdx subtabName groupedData = do
  let subtabId = "subtab-content-" <> show srcIdx <> "-" <> show subIdx
  H.div
    ! A.id (toValue subtabId)
    ! A.class_ "subtab-content"
    ! A.style "display: hidden;"
    $ case groupedData of
      Leaf transactions -> do
        let groupedTransactions = Map.singleton subtabName transactions
        generateAggregatedRowsWithExpandableDetails (toHtml subtabName) groupedTransactions srcIdx subtabName
      Node deeperLevels -> do
        if isFinalGrouping deeperLevels
          then do
            let formattedData = Map.map (\case Leaf txs -> txs; _ -> []) deeperLevels
            generateAggregatedRowsWithExpandableDetails (toHtml subtabName) formattedData srcIdx subtabName
          else do
            -- Otherwise, recurse normally
            H.table ! A.class_ "base-table hover-table striped-table w-full" $ do
              generateTableHeader (toHtml subtabName)

              H.tbody $
                forM_ (Prelude.zip [0 ..] (Map.toList deeperLevels)) $ \(localIdx, (groupLabel, nextLevel)) -> do
                  let (balance, withdrawals, deposites) = computeGroupTotals $ extractAllTransactions nextLevel
                  let sectionId =
                        T.concat
                          [ "details-",
                            T.pack (show srcIdx), -- e.g. "0"
                            "-",
                            T.pack (show subIdx), -- e.g. "1"
                            "-",
                            groupLabel, -- e.g. "Groceries"
                            "-",
                            T.pack (show localIdx) -- e.g. "0"
                          ]

                  generateAggregateRow groupLabel balance withdrawals deposites sectionId

                  H.tr ! A.id (toValue sectionId) ! A.class_ "hidden" $ do
                    H.td ! A.colspan "5" $
                      generateNestedTable srcIdx (read (show subIdx <> "1")) groupLabel nextLevel

              let (totalBalance, totalWithdrawals, totalDeposits) = computeGroupTotals $ extractAllTransactions groupedData
              generateTotalsRow totalWithdrawals totalDeposits totalBalance
  where
    isFinalGrouping :: Map.Map Text GroupedTransactions -> Bool
    isFinalGrouping deeperLevels =
      Prelude.all (\case Leaf _ -> True; _ -> False) (Map.elems deeperLevels)

generateSourceTables ::
  [Entity TransactionSource] ->
  Map.Map (Entity TransactionSource) [CategorizedTransaction] ->
  H.Html
generateSourceTables transactionSources aggregatedBySource = do
  H.div ! A.class_ "tabs-container flex flex-col" $ do
    generateButtonRow transactionSources subtabMappings

    H.div ! A.class_ "border border-primary p-2 rounded-md mt-4" $ do
      forM_ (Prelude.zip [0 ..] transactionSources) $ \(srcIdx, sourceEnt) -> do
        let tabId = "tab-content-" <> show srcIdx
            thisSourceData = Map.lookup sourceEnt aggregatedBySource

        H.div
          ! A.id (toValue tabId)
          ! H.dataAttribute "tab-index" (toValue srcIdx)
          ! A.class_ "tab-content"
          ! A.style (if srcIdx == 0 then "display: block;" else "display: hidden;")
          $ case thisSourceData of
            Just allTxs -> do
              forM_ (Prelude.zip [0 ..] subtabMappings) $ \(subIdx, (subName, groupFuncs)) -> do
                let groupedData = applyGroupingLevels allTxs groupFuncs
                generateNestedTable srcIdx subIdx subName groupedData
            Nothing -> H.p "No transactions for this source."

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
            then Just makeAddTransactionsBanner
            else Nothing

  let tabs = generateSourceTables transactionSources groupedBySource
  let strictText = generateHomapageHtml updatedBanner tabs
  return strictText
