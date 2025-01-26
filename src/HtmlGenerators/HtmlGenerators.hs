{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerators.HtmlGenerators
  ( renderTransactionsPage,
    renderSupportPage,
    renderPdfResultPage,
  )
where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Map hiding ((!), (!?))
import qualified Data.Map as Map hiding ((!), (!?))
import Data.Ord (comparing)
import Data.Text as T (Text, intercalate, pack, unlines)
import qualified Data.Text.Lazy as TL
import Data.Time
import Database.Models
import Database.Persist.Postgresql (BackendKey (SqlBackendKey), Entity (entityKey, entityVal), fromSqlKey)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Types (CategorizedTransaction (transaction, transactionId), category)

renderSupportPage :: Html
renderSupportPage =
  H.body $ do
    H.div ! A.class_ "container" $ do
      H.h1 "Support"
      H.p $ do
        "If you need assistance, please reach out to me via email: mcarroll1220@gmail.com"

renderPdfResultPage :: Text -> Text -> TL.Text
renderPdfResultPage filename rawText =
  renderHtml $ H.docTypeHtml $ do
    H.head $ do
      H.title "PDF Upload Result"
    H.body $ do
      H.h1 "PDF Uploaded Successfully!"
      H.p $ do
        "Filename: "
        H.b (toHtml filename)
      H.h2 "Extracted Text"
      H.pre (toHtml rawText)

renderTransactionsPage :: Entity UploadedPdf -> Map TransactionSourceId [Entity Category] -> [CategorizedTransaction] -> Html
renderTransactionsPage file categoryLookup txs =
  body $ do
    H.div ! A.class_ "w-full p-3" $ do
      H.h1 ! A.class_ "text-3xl font-semibold text-gray-900 mb-4" $
        toHtml $
          "Transactions for " <> uploadedPdfFilename (entityVal file)

      H.table ! A.class_ "base-table striped-table" $ do
        -- Table Header
        H.thead ! A.class_ "table-head" $ do
          H.tr $ do
            H.th ! A.class_ "p-2 border border-primary font-semibold" $ "Id"
            H.th ! A.class_ "p-2 border border-primary font-semibold" $ "Description"
            H.th ! A.class_ "p-2 border border-primary font-semibold" $ "Date"
            H.th ! A.class_ "p-2 border border-primary font-semibold" $ "Amount"
            H.th ! A.class_ "p-2 border border-primary font-semibold" $ "Kind"
            H.th ! A.class_ "p-2 border border-primary font-semibold" $ "Category"
            H.th ! A.class_ "p-2 border border-primary font-semibold" $ "Actions"

        -- Table Body
        H.tbody $ do
          mapM_ (renderEditableTransactionRow file categoryLookup) txs

renderEditableTransactionRow :: Entity UploadedPdf -> Map TransactionSourceId [Entity Category] -> CategorizedTransaction -> Html
renderEditableTransactionRow file categoryLookup tx = do
  let Transaction {transactionTransactionSourceId, transactionKind, transactionDescription, transactionDateOfTransaction, transactionAmount} = entityVal $ transaction tx
      currCatName = categoryName $ entityVal $ category tx
      catId = fromSqlKey $ entityKey $ category tx
      allCategories = Map.lookup transactionTransactionSourceId categoryLookup
      txId = case transactionId tx of
        Just (TransactionKey (SqlBackendKey key)) -> T.pack (show key)
        Nothing -> "Unknown"

  H.form
    ! A.method "post"
    ! A.action (toValue $ "/update-transaction/" <> txId)
    $ do
      H.tr
        ! A.id (H.toValue $ "tx-" <> txId)
        ! A.class_ "border border-gray-300 bg-white hover:bg-gray-50 transition"
        $ do
          -- Transaction ID
          H.td ! A.class_ "p-2 border border-gray-300 text-center" $
            toHtml txId

          -- Description
          H.td ! A.class_ "p-2 border border-gray-300" $
            H.input
              ! A.type_ "text"
              ! A.name "description"
              ! A.class_ "w-full border border-gray-300 rounded-md p-2"
              ! A.value (toValue transactionDescription)

          -- Date
          H.td ! A.class_ "p-2 border border-gray-300" $
            H.input
              ! A.type_ "date"
              ! A.name "transactionDate"
              ! A.class_ "w-full border border-gray-300 rounded-md p-2"
              ! A.value (toValue $ formatMonthYear transactionDateOfTransaction)

          -- Amount
          H.td ! A.class_ "p-2 border border-gray-300 text-right" $
            H.input
              ! A.type_ "number"
              ! A.step "0.01"
              ! A.name "amount"
              ! A.class_ "w-full border border-gray-300 rounded-md p-2 text-right"
              ! A.value (toValue $ show transactionAmount)

          -- Kind (Dropdown)
          H.td ! A.class_ "p-2 border border-gray-300 text-center"
            $ H.select
              ! A.name "kind"
              ! A.class_ "w-full border border-gray-300 rounded-md p-2"
            $ forM_ [Withdrawal, Deposit]
            $ \kind -> do
              H.option
                ! A.value (toValue $ show kind)
                ! (if transactionKind == kind then A.selected "selected" else mempty)
                $ toHtml
                $ show kind

          -- Category (Dropdown)
          H.td ! A.class_ "p-2 border border-gray-300 text-center"
            $ H.select
              ! A.name "category"
              ! A.class_ "w-full border border-gray-300 rounded-md p-2"
            $ do
              case allCategories of
                Just categories ->
                  forM_ categories $ \cat -> do
                    let currentCatId = fromSqlKey $ entityKey cat
                        currentCatName = categoryName (entityVal cat)
                    H.option
                      ! A.value (toValue currentCatId)
                      ! (if currentCatId == catId then A.selected "selected" else mempty)
                      $ toHtml currentCatName
                Nothing -> H.option ! A.value "" $ "No categories available"

          -- Hidden File ID
          H.input
            ! A.type_ "hidden"
            ! A.name "fileId"
            ! A.value (toValue $ fromSqlKey $ entityKey file)

          -- Actions
          H.td ! A.class_ "p-2 border border-gray-300 text-center" $
            H.input
              ! A.type_ "submit"
              ! A.class_ "primary-button"
              ! A.value "Save"

renderHtmlT :: Html -> TL.Text
renderHtmlT = renderHtml

formatMonthYear :: UTCTime -> T.Text
formatMonthYear utcTime = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" utcTime)
